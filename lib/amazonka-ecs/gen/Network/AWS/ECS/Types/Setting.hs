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
    setValue,
    setName,
    setPrincipalARN,
  )
where

import Network.AWS.ECS.Types.SettingName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The current account setting for a resource.
--
-- /See:/ 'mkSetting' smart constructor.
data Setting = Setting'
  { value :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe SettingName,
    principalARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Setting' with the minimum fields required to make a request.
--
-- * 'name' - The Amazon ECS resource name.
-- * 'principalARN' - The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the authenticated user is assumed.
-- * 'value' - Whether the account setting is enabled or disabled for the specified resource.
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
setValue :: Lens.Lens' Setting (Lude.Maybe Lude.Text)
setValue = Lens.lens (value :: Setting -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Setting)
{-# DEPRECATED setValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The Amazon ECS resource name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setName :: Lens.Lens' Setting (Lude.Maybe SettingName)
setName = Lens.lens (name :: Setting -> Lude.Maybe SettingName) (\s a -> s {name = a} :: Setting)
{-# DEPRECATED setName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ARN of the principal, which can be an IAM user, IAM role, or the root user. If this field is omitted, the authenticated user is assumed.
--
-- /Note:/ Consider using 'principalARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
setPrincipalARN :: Lens.Lens' Setting (Lude.Maybe Lude.Text)
setPrincipalARN = Lens.lens (principalARN :: Setting -> Lude.Maybe Lude.Text) (\s a -> s {principalARN = a} :: Setting)
{-# DEPRECATED setPrincipalARN "Use generic-lens or generic-optics with 'principalARN' instead." #-}

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
