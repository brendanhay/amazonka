{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Setting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Setting
  ( Setting (..),

    -- * Smart constructor
    mkSetting,

    -- * Lenses
    sfName,
    sfPrincipalArn,
    sfValue,
  )
where

import qualified Network.AWS.ECS.Types.SettingName as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The current account setting for a resource.
--
-- /See:/ 'mkSetting' smart constructor.
data Setting = Setting'
  { -- | The Amazon ECS resource name.
    name :: Core.Maybe Types.SettingName,
    -- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the authenticated user is assumed.
    principalArn :: Core.Maybe Types.String,
    -- | Whether the account setting is enabled or disabled for the specified resource.
    value :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Setting' value with any optional fields omitted.
mkSetting ::
  Setting
mkSetting =
  Setting'
    { name = Core.Nothing,
      principalArn = Core.Nothing,
      value = Core.Nothing
    }

-- | The Amazon ECS resource name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfName :: Lens.Lens' Setting (Core.Maybe Types.SettingName)
sfName = Lens.field @"name"
{-# DEPRECATED sfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the authenticated user is assumed.
--
-- /Note:/ Consider using 'principalArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfPrincipalArn :: Lens.Lens' Setting (Core.Maybe Types.String)
sfPrincipalArn = Lens.field @"principalArn"
{-# DEPRECATED sfPrincipalArn "Use generic-lens or generic-optics with 'principalArn' instead." #-}

-- | Whether the account setting is enabled or disabled for the specified resource.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValue :: Lens.Lens' Setting (Core.Maybe Types.String)
sfValue = Lens.field @"value"
{-# DEPRECATED sfValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Setting where
  parseJSON =
    Core.withObject "Setting" Core.$
      \x ->
        Setting'
          Core.<$> (x Core..:? "name")
          Core.<*> (x Core..:? "principalArn")
          Core.<*> (x Core..:? "value")
