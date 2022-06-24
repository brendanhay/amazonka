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
-- Module      : Amazonka.SageMaker.Types.AutoMLJobConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria
import Amazonka.SageMaker.Types.AutoMLSecurityConfig

-- | A collection of settings used for an AutoML job.
--
-- /See:/ 'newAutoMLJobConfig' smart constructor.
data AutoMLJobConfig = AutoMLJobConfig'
  { -- | How long an AutoML job is allowed to run, or how many candidates a job
    -- is allowed to generate.
    completionCriteria :: Prelude.Maybe AutoMLJobCompletionCriteria,
    -- | The security configuration for traffic encryption or Amazon VPC
    -- settings.
    securityConfig :: Prelude.Maybe AutoMLSecurityConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionCriteria', 'autoMLJobConfig_completionCriteria' - How long an AutoML job is allowed to run, or how many candidates a job
-- is allowed to generate.
--
-- 'securityConfig', 'autoMLJobConfig_securityConfig' - The security configuration for traffic encryption or Amazon VPC
-- settings.
newAutoMLJobConfig ::
  AutoMLJobConfig
newAutoMLJobConfig =
  AutoMLJobConfig'
    { completionCriteria =
        Prelude.Nothing,
      securityConfig = Prelude.Nothing
    }

-- | How long an AutoML job is allowed to run, or how many candidates a job
-- is allowed to generate.
autoMLJobConfig_completionCriteria :: Lens.Lens' AutoMLJobConfig (Prelude.Maybe AutoMLJobCompletionCriteria)
autoMLJobConfig_completionCriteria = Lens.lens (\AutoMLJobConfig' {completionCriteria} -> completionCriteria) (\s@AutoMLJobConfig' {} a -> s {completionCriteria = a} :: AutoMLJobConfig)

-- | The security configuration for traffic encryption or Amazon VPC
-- settings.
autoMLJobConfig_securityConfig :: Lens.Lens' AutoMLJobConfig (Prelude.Maybe AutoMLSecurityConfig)
autoMLJobConfig_securityConfig = Lens.lens (\AutoMLJobConfig' {securityConfig} -> securityConfig) (\s@AutoMLJobConfig' {} a -> s {securityConfig = a} :: AutoMLJobConfig)

instance Core.FromJSON AutoMLJobConfig where
  parseJSON =
    Core.withObject
      "AutoMLJobConfig"
      ( \x ->
          AutoMLJobConfig'
            Prelude.<$> (x Core..:? "CompletionCriteria")
            Prelude.<*> (x Core..:? "SecurityConfig")
      )

instance Prelude.Hashable AutoMLJobConfig where
  hashWithSalt _salt AutoMLJobConfig' {..} =
    _salt `Prelude.hashWithSalt` completionCriteria
      `Prelude.hashWithSalt` securityConfig

instance Prelude.NFData AutoMLJobConfig where
  rnf AutoMLJobConfig' {..} =
    Prelude.rnf completionCriteria
      `Prelude.seq` Prelude.rnf securityConfig

instance Core.ToJSON AutoMLJobConfig where
  toJSON AutoMLJobConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CompletionCriteria" Core..=)
              Prelude.<$> completionCriteria,
            ("SecurityConfig" Core..=)
              Prelude.<$> securityConfig
          ]
      )
