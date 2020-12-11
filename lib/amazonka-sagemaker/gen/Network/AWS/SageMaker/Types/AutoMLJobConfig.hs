-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobConfig
  ( AutoMLJobConfig (..),

    -- * Smart constructor
    mkAutoMLJobConfig,

    -- * Lenses
    amljcSecurityConfig,
    amljcCompletionCriteria,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
import Network.AWS.SageMaker.Types.AutoMLSecurityConfig

-- | A collection of settings used for a job.
--
-- /See:/ 'mkAutoMLJobConfig' smart constructor.
data AutoMLJobConfig = AutoMLJobConfig'
  { securityConfig ::
      Lude.Maybe AutoMLSecurityConfig,
    completionCriteria ::
      Lude.Maybe AutoMLJobCompletionCriteria
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLJobConfig' with the minimum fields required to make a request.
--
-- * 'completionCriteria' - How long a job is allowed to run, or how many candidates a job is allowed to generate.
-- * 'securityConfig' - Security configuration for traffic encryption or Amazon VPC settings.
mkAutoMLJobConfig ::
  AutoMLJobConfig
mkAutoMLJobConfig =
  AutoMLJobConfig'
    { securityConfig = Lude.Nothing,
      completionCriteria = Lude.Nothing
    }

-- | Security configuration for traffic encryption or Amazon VPC settings.
--
-- /Note:/ Consider using 'securityConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljcSecurityConfig :: Lens.Lens' AutoMLJobConfig (Lude.Maybe AutoMLSecurityConfig)
amljcSecurityConfig = Lens.lens (securityConfig :: AutoMLJobConfig -> Lude.Maybe AutoMLSecurityConfig) (\s a -> s {securityConfig = a} :: AutoMLJobConfig)
{-# DEPRECATED amljcSecurityConfig "Use generic-lens or generic-optics with 'securityConfig' instead." #-}

-- | How long a job is allowed to run, or how many candidates a job is allowed to generate.
--
-- /Note:/ Consider using 'completionCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amljcCompletionCriteria :: Lens.Lens' AutoMLJobConfig (Lude.Maybe AutoMLJobCompletionCriteria)
amljcCompletionCriteria = Lens.lens (completionCriteria :: AutoMLJobConfig -> Lude.Maybe AutoMLJobCompletionCriteria) (\s a -> s {completionCriteria = a} :: AutoMLJobConfig)
{-# DEPRECATED amljcCompletionCriteria "Use generic-lens or generic-optics with 'completionCriteria' instead." #-}

instance Lude.FromJSON AutoMLJobConfig where
  parseJSON =
    Lude.withObject
      "AutoMLJobConfig"
      ( \x ->
          AutoMLJobConfig'
            Lude.<$> (x Lude..:? "SecurityConfig")
            Lude.<*> (x Lude..:? "CompletionCriteria")
      )

instance Lude.ToJSON AutoMLJobConfig where
  toJSON AutoMLJobConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SecurityConfig" Lude..=) Lude.<$> securityConfig,
            ("CompletionCriteria" Lude..=) Lude.<$> completionCriteria
          ]
      )
