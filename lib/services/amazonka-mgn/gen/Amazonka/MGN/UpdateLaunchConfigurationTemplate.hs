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
-- Module      : Amazonka.MGN.UpdateLaunchConfigurationTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new ReplicationConfigurationTemplate.
module Amazonka.MGN.UpdateLaunchConfigurationTemplate
  ( -- * Creating a Request
    UpdateLaunchConfigurationTemplate (..),
    newUpdateLaunchConfigurationTemplate,

    -- * Request Lenses
    updateLaunchConfigurationTemplate_postLaunchActions,
    updateLaunchConfigurationTemplate_launchConfigurationTemplateID,

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
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLaunchConfigurationTemplate' smart constructor.
data UpdateLaunchConfigurationTemplate = UpdateLaunchConfigurationTemplate'
  { -- | Update Launch configuration Target instance right sizing request.
    postLaunchActions :: Prelude.Maybe PostLaunchActions,
    -- | Update Launch configuration Target instance right sizing request.
    launchConfigurationTemplateID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLaunchConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'postLaunchActions', 'updateLaunchConfigurationTemplate_postLaunchActions' - Update Launch configuration Target instance right sizing request.
--
-- 'launchConfigurationTemplateID', 'updateLaunchConfigurationTemplate_launchConfigurationTemplateID' - Update Launch configuration Target instance right sizing request.
newUpdateLaunchConfigurationTemplate ::
  -- | 'launchConfigurationTemplateID'
  Prelude.Text ->
  UpdateLaunchConfigurationTemplate
newUpdateLaunchConfigurationTemplate
  pLaunchConfigurationTemplateID_ =
    UpdateLaunchConfigurationTemplate'
      { postLaunchActions =
          Prelude.Nothing,
        launchConfigurationTemplateID =
          pLaunchConfigurationTemplateID_
      }

-- | Update Launch configuration Target instance right sizing request.
updateLaunchConfigurationTemplate_postLaunchActions :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe PostLaunchActions)
updateLaunchConfigurationTemplate_postLaunchActions = Lens.lens (\UpdateLaunchConfigurationTemplate' {postLaunchActions} -> postLaunchActions) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {postLaunchActions = a} :: UpdateLaunchConfigurationTemplate)

-- | Update Launch configuration Target instance right sizing request.
updateLaunchConfigurationTemplate_launchConfigurationTemplateID :: Lens.Lens' UpdateLaunchConfigurationTemplate Prelude.Text
updateLaunchConfigurationTemplate_launchConfigurationTemplateID = Lens.lens (\UpdateLaunchConfigurationTemplate' {launchConfigurationTemplateID} -> launchConfigurationTemplateID) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {launchConfigurationTemplateID = a} :: UpdateLaunchConfigurationTemplate)

instance
  Core.AWSRequest
    UpdateLaunchConfigurationTemplate
  where
  type
    AWSResponse UpdateLaunchConfigurationTemplate =
      LaunchConfigurationTemplate
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    UpdateLaunchConfigurationTemplate
  where
  hashWithSalt
    _salt
    UpdateLaunchConfigurationTemplate' {..} =
      _salt `Prelude.hashWithSalt` postLaunchActions
        `Prelude.hashWithSalt` launchConfigurationTemplateID

instance
  Prelude.NFData
    UpdateLaunchConfigurationTemplate
  where
  rnf UpdateLaunchConfigurationTemplate' {..} =
    Prelude.rnf postLaunchActions
      `Prelude.seq` Prelude.rnf launchConfigurationTemplateID

instance
  Data.ToHeaders
    UpdateLaunchConfigurationTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateLaunchConfigurationTemplate
  where
  toJSON UpdateLaunchConfigurationTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("postLaunchActions" Data..=)
              Prelude.<$> postLaunchActions,
            Prelude.Just
              ( "launchConfigurationTemplateID"
                  Data..= launchConfigurationTemplateID
              )
          ]
      )

instance
  Data.ToPath
    UpdateLaunchConfigurationTemplate
  where
  toPath =
    Prelude.const "/UpdateLaunchConfigurationTemplate"

instance
  Data.ToQuery
    UpdateLaunchConfigurationTemplate
  where
  toQuery = Prelude.const Prelude.mempty
