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
-- Module      : Amazonka.AppConfig.CreateEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an environment. For each application, you define one or more
-- environments. An environment is a deployment group of AppConfig targets,
-- such as applications in a @Beta@ or @Production@ environment. You can
-- also define environments for application subcomponents such as the
-- @Web@, @Mobile@ and @Back-end@ components for your application. You can
-- configure Amazon CloudWatch alarms for each environment. The system
-- monitors alarms during a configuration deployment. If an alarm is
-- triggered, the system rolls back the configuration.
module Amazonka.AppConfig.CreateEnvironment
  ( -- * Creating a Request
    CreateEnvironment (..),
    newCreateEnvironment,

    -- * Request Lenses
    createEnvironment_description,
    createEnvironment_monitors,
    createEnvironment_tags,
    createEnvironment_applicationId,
    createEnvironment_name,

    -- * Destructuring the Response
    Environment (..),
    newEnvironment,

    -- * Response Lenses
    environment_applicationId,
    environment_description,
    environment_id,
    environment_monitors,
    environment_name,
    environment_state,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEnvironment' smart constructor.
data CreateEnvironment = CreateEnvironment'
  { -- | A description of the environment.
    description :: Prelude.Maybe Prelude.Text,
    -- | Amazon CloudWatch alarms to monitor during the deployment process.
    monitors :: Prelude.Maybe [Monitor],
    -- | Metadata to assign to the environment. Tags help organize and categorize
    -- your AppConfig resources. Each tag consists of a key and an optional
    -- value, both of which you define.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The application ID.
    applicationId :: Prelude.Text,
    -- | A name for the environment.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createEnvironment_description' - A description of the environment.
--
-- 'monitors', 'createEnvironment_monitors' - Amazon CloudWatch alarms to monitor during the deployment process.
--
-- 'tags', 'createEnvironment_tags' - Metadata to assign to the environment. Tags help organize and categorize
-- your AppConfig resources. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- 'applicationId', 'createEnvironment_applicationId' - The application ID.
--
-- 'name', 'createEnvironment_name' - A name for the environment.
newCreateEnvironment ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateEnvironment
newCreateEnvironment pApplicationId_ pName_ =
  CreateEnvironment'
    { description = Prelude.Nothing,
      monitors = Prelude.Nothing,
      tags = Prelude.Nothing,
      applicationId = pApplicationId_,
      name = pName_
    }

-- | A description of the environment.
createEnvironment_description :: Lens.Lens' CreateEnvironment (Prelude.Maybe Prelude.Text)
createEnvironment_description = Lens.lens (\CreateEnvironment' {description} -> description) (\s@CreateEnvironment' {} a -> s {description = a} :: CreateEnvironment)

-- | Amazon CloudWatch alarms to monitor during the deployment process.
createEnvironment_monitors :: Lens.Lens' CreateEnvironment (Prelude.Maybe [Monitor])
createEnvironment_monitors = Lens.lens (\CreateEnvironment' {monitors} -> monitors) (\s@CreateEnvironment' {} a -> s {monitors = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | Metadata to assign to the environment. Tags help organize and categorize
-- your AppConfig resources. Each tag consists of a key and an optional
-- value, both of which you define.
createEnvironment_tags :: Lens.Lens' CreateEnvironment (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createEnvironment_tags = Lens.lens (\CreateEnvironment' {tags} -> tags) (\s@CreateEnvironment' {} a -> s {tags = a} :: CreateEnvironment) Prelude.. Lens.mapping Lens.coerced

-- | The application ID.
createEnvironment_applicationId :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_applicationId = Lens.lens (\CreateEnvironment' {applicationId} -> applicationId) (\s@CreateEnvironment' {} a -> s {applicationId = a} :: CreateEnvironment)

-- | A name for the environment.
createEnvironment_name :: Lens.Lens' CreateEnvironment Prelude.Text
createEnvironment_name = Lens.lens (\CreateEnvironment' {name} -> name) (\s@CreateEnvironment' {} a -> s {name = a} :: CreateEnvironment)

instance Core.AWSRequest CreateEnvironment where
  type AWSResponse CreateEnvironment = Environment
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateEnvironment where
  hashWithSalt _salt CreateEnvironment' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` monitors
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateEnvironment where
  rnf CreateEnvironment' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf monitors
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEnvironment where
  toJSON CreateEnvironment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Monitors" Data..=) Prelude.<$> monitors,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateEnvironment where
  toPath CreateEnvironment' {..} =
    Prelude.mconcat
      [ "/applications/",
        Data.toBS applicationId,
        "/environments"
      ]

instance Data.ToQuery CreateEnvironment where
  toQuery = Prelude.const Prelude.mempty
