{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
import Network.AWS.SageMaker.Types.AutoMLSecurityConfig

-- | A collection of settings used for a job.
--
-- /See:/ 'newAutoMLJobConfig' smart constructor.
data AutoMLJobConfig = AutoMLJobConfig'
  { -- | Security configuration for traffic encryption or Amazon VPC settings.
    securityConfig :: Prelude.Maybe AutoMLSecurityConfig,
    -- | How long a job is allowed to run, or how many candidates a job is
    -- allowed to generate.
    completionCriteria :: Prelude.Maybe AutoMLJobCompletionCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { securityConfig = Prelude.Nothing,
      completionCriteria = Prelude.Nothing
    }

-- | Security configuration for traffic encryption or Amazon VPC settings.
autoMLJobConfig_securityConfig :: Lens.Lens' AutoMLJobConfig (Prelude.Maybe AutoMLSecurityConfig)
autoMLJobConfig_securityConfig = Lens.lens (\AutoMLJobConfig' {securityConfig} -> securityConfig) (\s@AutoMLJobConfig' {} a -> s {securityConfig = a} :: AutoMLJobConfig)

-- | How long a job is allowed to run, or how many candidates a job is
-- allowed to generate.
autoMLJobConfig_completionCriteria :: Lens.Lens' AutoMLJobConfig (Prelude.Maybe AutoMLJobCompletionCriteria)
autoMLJobConfig_completionCriteria = Lens.lens (\AutoMLJobConfig' {completionCriteria} -> completionCriteria) (\s@AutoMLJobConfig' {} a -> s {completionCriteria = a} :: AutoMLJobConfig)

instance Prelude.FromJSON AutoMLJobConfig where
  parseJSON =
    Prelude.withObject
      "AutoMLJobConfig"
      ( \x ->
          AutoMLJobConfig'
            Prelude.<$> (x Prelude..:? "SecurityConfig")
            Prelude.<*> (x Prelude..:? "CompletionCriteria")
      )

instance Prelude.Hashable AutoMLJobConfig

instance Prelude.NFData AutoMLJobConfig

instance Prelude.ToJSON AutoMLJobConfig where
  toJSON AutoMLJobConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SecurityConfig" Prelude..=)
              Prelude.<$> securityConfig,
            ("CompletionCriteria" Prelude..=)
              Prelude.<$> completionCriteria
          ]
      )
