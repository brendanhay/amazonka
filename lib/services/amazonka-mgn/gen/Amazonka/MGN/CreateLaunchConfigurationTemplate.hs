{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MGN.CreateLaunchConfigurationTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new ReplicationConfigurationTemplate.
module Amazonka.MGN.CreateLaunchConfigurationTemplate
  ( -- * Creating a Request
    CreateLaunchConfigurationTemplate (..),
    newCreateLaunchConfigurationTemplate,

    -- * Request Lenses
    createLaunchConfigurationTemplate_tags,
    createLaunchConfigurationTemplate_postLaunchActions,

    -- * Destructuring the Response
    LaunchConfigurationTemplate (..),
    newLaunchConfigurationTemplate,

    -- * Response Lenses
    launchConfigurationTemplate_tags,
    launchConfigurationTemplate_arn,
    launchConfigurationTemplate_postLaunchActions,
    launchConfigurationTemplate_launchConfigurationTemplateID,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLaunchConfigurationTemplate' smart constructor.
data CreateLaunchConfigurationTemplate = CreateLaunchConfigurationTemplate'
  { -- | Request to associate the default Application Migration Service Security
    -- group with the Replication Settings template.
    tags :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Request to associate the default Application Migration Service Security
    -- group with the Replication Settings template.
    postLaunchActions :: Prelude.Maybe PostLaunchActions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLaunchConfigurationTemplate_tags' - Request to associate the default Application Migration Service Security
-- group with the Replication Settings template.
--
-- 'postLaunchActions', 'createLaunchConfigurationTemplate_postLaunchActions' - Request to associate the default Application Migration Service Security
-- group with the Replication Settings template.
newCreateLaunchConfigurationTemplate ::
  CreateLaunchConfigurationTemplate
newCreateLaunchConfigurationTemplate =
  CreateLaunchConfigurationTemplate'
    { tags =
        Prelude.Nothing,
      postLaunchActions = Prelude.Nothing
    }

-- | Request to associate the default Application Migration Service Security
-- group with the Replication Settings template.
createLaunchConfigurationTemplate_tags :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLaunchConfigurationTemplate_tags = Lens.lens (\CreateLaunchConfigurationTemplate' {tags} -> tags) (\s@CreateLaunchConfigurationTemplate' {} a -> s {tags = a} :: CreateLaunchConfigurationTemplate) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | Request to associate the default Application Migration Service Security
-- group with the Replication Settings template.
createLaunchConfigurationTemplate_postLaunchActions :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe PostLaunchActions)
createLaunchConfigurationTemplate_postLaunchActions = Lens.lens (\CreateLaunchConfigurationTemplate' {postLaunchActions} -> postLaunchActions) (\s@CreateLaunchConfigurationTemplate' {} a -> s {postLaunchActions = a} :: CreateLaunchConfigurationTemplate)

instance
  Core.AWSRequest
    CreateLaunchConfigurationTemplate
  where
  type
    AWSResponse CreateLaunchConfigurationTemplate =
      LaunchConfigurationTemplate
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance
  Prelude.Hashable
    CreateLaunchConfigurationTemplate
  where
  hashWithSalt
    _salt
    CreateLaunchConfigurationTemplate' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` postLaunchActions

instance
  Prelude.NFData
    CreateLaunchConfigurationTemplate
  where
  rnf CreateLaunchConfigurationTemplate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf postLaunchActions

instance
  Core.ToHeaders
    CreateLaunchConfigurationTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    CreateLaunchConfigurationTemplate
  where
  toJSON CreateLaunchConfigurationTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("postLaunchActions" Core..=)
              Prelude.<$> postLaunchActions
          ]
      )

instance
  Core.ToPath
    CreateLaunchConfigurationTemplate
  where
  toPath =
    Prelude.const "/CreateLaunchConfigurationTemplate"

instance
  Core.ToQuery
    CreateLaunchConfigurationTemplate
  where
  toQuery = Prelude.const Prelude.mempty
