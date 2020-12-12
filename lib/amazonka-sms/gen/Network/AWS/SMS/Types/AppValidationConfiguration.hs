{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppValidationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppValidationConfiguration
  ( AppValidationConfiguration (..),

    -- * Smart constructor
    mkAppValidationConfiguration,

    -- * Lenses
    avcSsmValidationParameters,
    avcName,
    avcValidationId,
    avcAppValidationStrategy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SMS.Types.AppValidationStrategy
import Network.AWS.SMS.Types.SSMValidationParameters

-- | Configuration for validating an application.
--
-- /See:/ 'mkAppValidationConfiguration' smart constructor.
data AppValidationConfiguration = AppValidationConfiguration'
  { ssmValidationParameters ::
      Lude.Maybe SSMValidationParameters,
    name :: Lude.Maybe Lude.Text,
    validationId :: Lude.Maybe Lude.Text,
    appValidationStrategy ::
      Lude.Maybe AppValidationStrategy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AppValidationConfiguration' with the minimum fields required to make a request.
--
-- * 'appValidationStrategy' - The validation strategy.
-- * 'name' - The name of the configuration.
-- * 'ssmValidationParameters' - The validation parameters.
-- * 'validationId' - The ID of the validation.
mkAppValidationConfiguration ::
  AppValidationConfiguration
mkAppValidationConfiguration =
  AppValidationConfiguration'
    { ssmValidationParameters =
        Lude.Nothing,
      name = Lude.Nothing,
      validationId = Lude.Nothing,
      appValidationStrategy = Lude.Nothing
    }

-- | The validation parameters.
--
-- /Note:/ Consider using 'ssmValidationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcSsmValidationParameters :: Lens.Lens' AppValidationConfiguration (Lude.Maybe SSMValidationParameters)
avcSsmValidationParameters = Lens.lens (ssmValidationParameters :: AppValidationConfiguration -> Lude.Maybe SSMValidationParameters) (\s a -> s {ssmValidationParameters = a} :: AppValidationConfiguration)
{-# DEPRECATED avcSsmValidationParameters "Use generic-lens or generic-optics with 'ssmValidationParameters' instead." #-}

-- | The name of the configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcName :: Lens.Lens' AppValidationConfiguration (Lude.Maybe Lude.Text)
avcName = Lens.lens (name :: AppValidationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AppValidationConfiguration)
{-# DEPRECATED avcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the validation.
--
-- /Note:/ Consider using 'validationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcValidationId :: Lens.Lens' AppValidationConfiguration (Lude.Maybe Lude.Text)
avcValidationId = Lens.lens (validationId :: AppValidationConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {validationId = a} :: AppValidationConfiguration)
{-# DEPRECATED avcValidationId "Use generic-lens or generic-optics with 'validationId' instead." #-}

-- | The validation strategy.
--
-- /Note:/ Consider using 'appValidationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcAppValidationStrategy :: Lens.Lens' AppValidationConfiguration (Lude.Maybe AppValidationStrategy)
avcAppValidationStrategy = Lens.lens (appValidationStrategy :: AppValidationConfiguration -> Lude.Maybe AppValidationStrategy) (\s a -> s {appValidationStrategy = a} :: AppValidationConfiguration)
{-# DEPRECATED avcAppValidationStrategy "Use generic-lens or generic-optics with 'appValidationStrategy' instead." #-}

instance Lude.FromJSON AppValidationConfiguration where
  parseJSON =
    Lude.withObject
      "AppValidationConfiguration"
      ( \x ->
          AppValidationConfiguration'
            Lude.<$> (x Lude..:? "ssmValidationParameters")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "validationId")
            Lude.<*> (x Lude..:? "appValidationStrategy")
      )

instance Lude.ToJSON AppValidationConfiguration where
  toJSON AppValidationConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ssmValidationParameters" Lude..=)
              Lude.<$> ssmValidationParameters,
            ("name" Lude..=) Lude.<$> name,
            ("validationId" Lude..=) Lude.<$> validationId,
            ("appValidationStrategy" Lude..=) Lude.<$> appValidationStrategy
          ]
      )
