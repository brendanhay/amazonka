{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.UserDataValidationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.UserDataValidationParameters
  ( UserDataValidationParameters (..),

    -- * Smart constructor
    mkUserDataValidationParameters,

    -- * Lenses
    udvpScriptType,
    udvpSource,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SMS.Types.ScriptType as Types
import qualified Network.AWS.SMS.Types.Source as Types

-- | Contains validation parameters.
--
-- /See:/ 'mkUserDataValidationParameters' smart constructor.
data UserDataValidationParameters = UserDataValidationParameters'
  { -- | The type of validation script.
    scriptType :: Core.Maybe Types.ScriptType,
    -- | The location of the validation script.
    source :: Core.Maybe Types.Source
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UserDataValidationParameters' value with any optional fields omitted.
mkUserDataValidationParameters ::
  UserDataValidationParameters
mkUserDataValidationParameters =
  UserDataValidationParameters'
    { scriptType = Core.Nothing,
      source = Core.Nothing
    }

-- | The type of validation script.
--
-- /Note:/ Consider using 'scriptType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvpScriptType :: Lens.Lens' UserDataValidationParameters (Core.Maybe Types.ScriptType)
udvpScriptType = Lens.field @"scriptType"
{-# DEPRECATED udvpScriptType "Use generic-lens or generic-optics with 'scriptType' instead." #-}

-- | The location of the validation script.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvpSource :: Lens.Lens' UserDataValidationParameters (Core.Maybe Types.Source)
udvpSource = Lens.field @"source"
{-# DEPRECATED udvpSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Core.FromJSON UserDataValidationParameters where
  toJSON UserDataValidationParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("scriptType" Core..=) Core.<$> scriptType,
            ("source" Core..=) Core.<$> source
          ]
      )

instance Core.FromJSON UserDataValidationParameters where
  parseJSON =
    Core.withObject "UserDataValidationParameters" Core.$
      \x ->
        UserDataValidationParameters'
          Core.<$> (x Core..:? "scriptType") Core.<*> (x Core..:? "source")
