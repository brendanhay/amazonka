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
    sfValue,
    sfName,
    sfPrincipalARN,
  )
where

import Network.AWS.ECS.Types.SettingName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The current account setting for a resource.
--
-- /See:/ 'mkSetting' smart constructor.
data Setting = Setting'
  { -- | Whether the account setting is enabled or disabled for the specified resource.
    value :: Lude.Maybe Lude.Text,
    -- | The Amazon ECS resource name.
    name :: Lude.Maybe SettingName,
    -- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the authenticated user is assumed.
    principalARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Setting' with the minimum fields required to make a request.
--
-- * 'value' - Whether the account setting is enabled or disabled for the specified resource.
-- * 'name' - The Amazon ECS resource name.
-- * 'principalARN' - The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the authenticated user is assumed.
mkSetting ::
  Setting
mkSetting =
  Setting'
    { value = Lude.Nothing,
      name = Lude.Nothing,
      principalARN = Lude.Nothing
    }

-- | Whether the account setting is enabled or disabled for the specified resource.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValue :: Lens.Lens' Setting (Lude.Maybe Lude.Text)
sfValue = Lens.lens (value :: Setting -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Setting)
{-# DEPRECATED sfValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The Amazon ECS resource name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfName :: Lens.Lens' Setting (Lude.Maybe SettingName)
sfName = Lens.lens (name :: Setting -> Lude.Maybe SettingName) (\s a -> s {name = a} :: Setting)
{-# DEPRECATED sfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the authenticated user is assumed.
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfPrincipalARN :: Lens.Lens' Setting (Lude.Maybe Lude.Text)
sfPrincipalARN = Lens.lens (principalARN :: Setting -> Lude.Maybe Lude.Text) (\s a -> s {principalARN = a} :: Setting)
{-# DEPRECATED sfPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

instance Lude.FromJSON Setting where
  parseJSON =
    Lude.withObject
      "Setting"
      ( \x ->
          Setting'
            Lude.<$> (x Lude..:? "value")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "principalArn")
      )
