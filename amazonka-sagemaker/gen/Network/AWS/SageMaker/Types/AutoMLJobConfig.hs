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
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
import Network.AWS.SageMaker.Types.AutoMLSecurityConfig

-- | A collection of settings used for a job.
--
-- /See:/ 'newAutoMLJobConfig' smart constructor.
data AutoMLJobConfig = AutoMLJobConfig'
  { -- | Security configuration for traffic encryption or Amazon VPC settings.
    securityConfig :: Core.Maybe AutoMLSecurityConfig,
    -- | How long a job is allowed to run, or how many candidates a job is
    -- allowed to generate.
    completionCriteria :: Core.Maybe AutoMLJobCompletionCriteria
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoMLJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityConfig', 'autoMLJobConfig_securityConfig' - Security configuration for traffic encryption or Amazon VPC settings.
--
-- 'completionCriteria', 'autoMLJobConfig_completionCriteria' - How long a job is allowed to run, or how many candidates a job is
-- allowed to generate.
newAutoMLJobConfig ::
  AutoMLJobConfig
newAutoMLJobConfig =
  AutoMLJobConfig'
    { securityConfig = Core.Nothing,
      completionCriteria = Core.Nothing
    }

-- | Security configuration for traffic encryption or Amazon VPC settings.
autoMLJobConfig_securityConfig :: Lens.Lens' AutoMLJobConfig (Core.Maybe AutoMLSecurityConfig)
autoMLJobConfig_securityConfig = Lens.lens (\AutoMLJobConfig' {securityConfig} -> securityConfig) (\s@AutoMLJobConfig' {} a -> s {securityConfig = a} :: AutoMLJobConfig)

-- | How long a job is allowed to run, or how many candidates a job is
-- allowed to generate.
autoMLJobConfig_completionCriteria :: Lens.Lens' AutoMLJobConfig (Core.Maybe AutoMLJobCompletionCriteria)
autoMLJobConfig_completionCriteria = Lens.lens (\AutoMLJobConfig' {completionCriteria} -> completionCriteria) (\s@AutoMLJobConfig' {} a -> s {completionCriteria = a} :: AutoMLJobConfig)

instance Core.FromJSON AutoMLJobConfig where
  parseJSON =
    Core.withObject
      "AutoMLJobConfig"
      ( \x ->
          AutoMLJobConfig'
            Core.<$> (x Core..:? "SecurityConfig")
            Core.<*> (x Core..:? "CompletionCriteria")
      )

instance Core.Hashable AutoMLJobConfig

instance Core.NFData AutoMLJobConfig

instance Core.ToJSON AutoMLJobConfig where
  toJSON AutoMLJobConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SecurityConfig" Core..=) Core.<$> securityConfig,
            ("CompletionCriteria" Core..=)
              Core.<$> completionCriteria
          ]
      )
