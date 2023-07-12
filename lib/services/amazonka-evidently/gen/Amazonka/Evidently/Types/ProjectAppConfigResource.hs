{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Evidently.Types.ProjectAppConfigResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ProjectAppConfigResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This is a structure that defines the configuration of how your
-- application integrates with AppConfig to run client-side evaluation.
--
-- /See:/ 'newProjectAppConfigResource' smart constructor.
data ProjectAppConfigResource = ProjectAppConfigResource'
  { -- | The ID of the AppConfig application to use for client-side evaluation.
    applicationId :: Prelude.Text,
    -- | The ID of the AppConfig profile to use for client-side evaluation.
    configurationProfileId :: Prelude.Text,
    -- | The ID of the AppConfig environment to use for client-side evaluation.
    -- This must be an environment that is within the application that you
    -- specify for @applicationId@.
    environmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectAppConfigResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'projectAppConfigResource_applicationId' - The ID of the AppConfig application to use for client-side evaluation.
--
-- 'configurationProfileId', 'projectAppConfigResource_configurationProfileId' - The ID of the AppConfig profile to use for client-side evaluation.
--
-- 'environmentId', 'projectAppConfigResource_environmentId' - The ID of the AppConfig environment to use for client-side evaluation.
-- This must be an environment that is within the application that you
-- specify for @applicationId@.
newProjectAppConfigResource ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'configurationProfileId'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  ProjectAppConfigResource
newProjectAppConfigResource
  pApplicationId_
  pConfigurationProfileId_
  pEnvironmentId_ =
    ProjectAppConfigResource'
      { applicationId =
          pApplicationId_,
        configurationProfileId = pConfigurationProfileId_,
        environmentId = pEnvironmentId_
      }

-- | The ID of the AppConfig application to use for client-side evaluation.
projectAppConfigResource_applicationId :: Lens.Lens' ProjectAppConfigResource Prelude.Text
projectAppConfigResource_applicationId = Lens.lens (\ProjectAppConfigResource' {applicationId} -> applicationId) (\s@ProjectAppConfigResource' {} a -> s {applicationId = a} :: ProjectAppConfigResource)

-- | The ID of the AppConfig profile to use for client-side evaluation.
projectAppConfigResource_configurationProfileId :: Lens.Lens' ProjectAppConfigResource Prelude.Text
projectAppConfigResource_configurationProfileId = Lens.lens (\ProjectAppConfigResource' {configurationProfileId} -> configurationProfileId) (\s@ProjectAppConfigResource' {} a -> s {configurationProfileId = a} :: ProjectAppConfigResource)

-- | The ID of the AppConfig environment to use for client-side evaluation.
-- This must be an environment that is within the application that you
-- specify for @applicationId@.
projectAppConfigResource_environmentId :: Lens.Lens' ProjectAppConfigResource Prelude.Text
projectAppConfigResource_environmentId = Lens.lens (\ProjectAppConfigResource' {environmentId} -> environmentId) (\s@ProjectAppConfigResource' {} a -> s {environmentId = a} :: ProjectAppConfigResource)

instance Data.FromJSON ProjectAppConfigResource where
  parseJSON =
    Data.withObject
      "ProjectAppConfigResource"
      ( \x ->
          ProjectAppConfigResource'
            Prelude.<$> (x Data..: "applicationId")
            Prelude.<*> (x Data..: "configurationProfileId")
            Prelude.<*> (x Data..: "environmentId")
      )

instance Prelude.Hashable ProjectAppConfigResource where
  hashWithSalt _salt ProjectAppConfigResource' {..} =
    _salt
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` configurationProfileId
      `Prelude.hashWithSalt` environmentId

instance Prelude.NFData ProjectAppConfigResource where
  rnf ProjectAppConfigResource' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf configurationProfileId
      `Prelude.seq` Prelude.rnf environmentId
