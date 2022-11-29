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
-- Module      : Amazonka.Evidently.Types.ProjectAppConfigResourceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ProjectAppConfigResourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Use this parameter to configure client-side evaluation for your project.
-- Client-side evaluation allows your application to assign variations to
-- user sessions locally instead of by calling the
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html EvaluateFeature>
-- operation to assign the variations. This mitigates the latency and
-- availability risks that come with an API call.
--
-- @ProjectAppConfigResource@ is a structure that defines the configuration
-- of how your application integrates with AppConfig to run client-side
-- evaluation.
--
-- /See:/ 'newProjectAppConfigResourceConfig' smart constructor.
data ProjectAppConfigResourceConfig = ProjectAppConfigResourceConfig'
  { -- | The ID of the AppConfig environment to use for client-side evaluation.
    -- This must be an environment that is within the application that you
    -- specify for @applicationId@.
    environmentId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AppConfig application to use for client-side evaluation.
    applicationId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectAppConfigResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentId', 'projectAppConfigResourceConfig_environmentId' - The ID of the AppConfig environment to use for client-side evaluation.
-- This must be an environment that is within the application that you
-- specify for @applicationId@.
--
-- 'applicationId', 'projectAppConfigResourceConfig_applicationId' - The ID of the AppConfig application to use for client-side evaluation.
newProjectAppConfigResourceConfig ::
  ProjectAppConfigResourceConfig
newProjectAppConfigResourceConfig =
  ProjectAppConfigResourceConfig'
    { environmentId =
        Prelude.Nothing,
      applicationId = Prelude.Nothing
    }

-- | The ID of the AppConfig environment to use for client-side evaluation.
-- This must be an environment that is within the application that you
-- specify for @applicationId@.
projectAppConfigResourceConfig_environmentId :: Lens.Lens' ProjectAppConfigResourceConfig (Prelude.Maybe Prelude.Text)
projectAppConfigResourceConfig_environmentId = Lens.lens (\ProjectAppConfigResourceConfig' {environmentId} -> environmentId) (\s@ProjectAppConfigResourceConfig' {} a -> s {environmentId = a} :: ProjectAppConfigResourceConfig)

-- | The ID of the AppConfig application to use for client-side evaluation.
projectAppConfigResourceConfig_applicationId :: Lens.Lens' ProjectAppConfigResourceConfig (Prelude.Maybe Prelude.Text)
projectAppConfigResourceConfig_applicationId = Lens.lens (\ProjectAppConfigResourceConfig' {applicationId} -> applicationId) (\s@ProjectAppConfigResourceConfig' {} a -> s {applicationId = a} :: ProjectAppConfigResourceConfig)

instance
  Prelude.Hashable
    ProjectAppConfigResourceConfig
  where
  hashWithSalt
    _salt
    ProjectAppConfigResourceConfig' {..} =
      _salt `Prelude.hashWithSalt` environmentId
        `Prelude.hashWithSalt` applicationId

instance
  Prelude.NFData
    ProjectAppConfigResourceConfig
  where
  rnf ProjectAppConfigResourceConfig' {..} =
    Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToJSON ProjectAppConfigResourceConfig where
  toJSON ProjectAppConfigResourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("environmentId" Core..=) Prelude.<$> environmentId,
            ("applicationId" Core..=) Prelude.<$> applicationId
          ]
      )
