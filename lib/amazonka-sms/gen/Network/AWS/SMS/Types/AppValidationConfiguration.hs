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
    avcAppValidationStrategy,
    avcName,
    avcSsmValidationParameters,
    avcValidationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.AppValidationStrategy as Types
import qualified Network.AWS.SMS.Types.NonEmptyStringWithMaxLen255 as Types
import qualified Network.AWS.SMS.Types.SSMValidationParameters as Types
import qualified Network.AWS.SMS.Types.ValidationId as Types

-- | Configuration for validating an application.
--
-- /See:/ 'mkAppValidationConfiguration' smart constructor.
data AppValidationConfiguration = AppValidationConfiguration'
  { -- | The validation strategy.
    appValidationStrategy :: Core.Maybe Types.AppValidationStrategy,
    -- | The name of the configuration.
    name :: Core.Maybe Types.NonEmptyStringWithMaxLen255,
    -- | The validation parameters.
    ssmValidationParameters :: Core.Maybe Types.SSMValidationParameters,
    -- | The ID of the validation.
    validationId :: Core.Maybe Types.ValidationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AppValidationConfiguration' value with any optional fields omitted.
mkAppValidationConfiguration ::
  AppValidationConfiguration
mkAppValidationConfiguration =
  AppValidationConfiguration'
    { appValidationStrategy = Core.Nothing,
      name = Core.Nothing,
      ssmValidationParameters = Core.Nothing,
      validationId = Core.Nothing
    }

-- | The validation strategy.
--
-- /Note:/ Consider using 'appValidationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcAppValidationStrategy :: Lens.Lens' AppValidationConfiguration (Core.Maybe Types.AppValidationStrategy)
avcAppValidationStrategy = Lens.field @"appValidationStrategy"
{-# DEPRECATED avcAppValidationStrategy "Use generic-lens or generic-optics with 'appValidationStrategy' instead." #-}

-- | The name of the configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcName :: Lens.Lens' AppValidationConfiguration (Core.Maybe Types.NonEmptyStringWithMaxLen255)
avcName = Lens.field @"name"
{-# DEPRECATED avcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The validation parameters.
--
-- /Note:/ Consider using 'ssmValidationParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcSsmValidationParameters :: Lens.Lens' AppValidationConfiguration (Core.Maybe Types.SSMValidationParameters)
avcSsmValidationParameters = Lens.field @"ssmValidationParameters"
{-# DEPRECATED avcSsmValidationParameters "Use generic-lens or generic-optics with 'ssmValidationParameters' instead." #-}

-- | The ID of the validation.
--
-- /Note:/ Consider using 'validationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcValidationId :: Lens.Lens' AppValidationConfiguration (Core.Maybe Types.ValidationId)
avcValidationId = Lens.field @"validationId"
{-# DEPRECATED avcValidationId "Use generic-lens or generic-optics with 'validationId' instead." #-}

instance Core.FromJSON AppValidationConfiguration where
  toJSON AppValidationConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("appValidationStrategy" Core..=) Core.<$> appValidationStrategy,
            ("name" Core..=) Core.<$> name,
            ("ssmValidationParameters" Core..=)
              Core.<$> ssmValidationParameters,
            ("validationId" Core..=) Core.<$> validationId
          ]
      )

instance Core.FromJSON AppValidationConfiguration where
  parseJSON =
    Core.withObject "AppValidationConfiguration" Core.$
      \x ->
        AppValidationConfiguration'
          Core.<$> (x Core..:? "appValidationStrategy")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "ssmValidationParameters")
          Core.<*> (x Core..:? "validationId")
