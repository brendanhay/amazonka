{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.EnvironmentVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.EnvironmentVariable
  ( EnvironmentVariable (..),

    -- * Smart constructor
    mkEnvironmentVariable,

    -- * Lenses
    evSecure,
    evKey,
    evValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an app's environment variable.
--
-- /See:/ 'mkEnvironmentVariable' smart constructor.
data EnvironmentVariable = EnvironmentVariable'
  { secure ::
      Lude.Maybe Lude.Bool,
    key :: Lude.Text,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentVariable' with the minimum fields required to make a request.
--
-- * 'key' - (Required) The environment variable's name, which can consist of up to 64 characters and must be specified. The name can contain upper- and lowercase letters, numbers, and underscores (_), but it must start with a letter or underscore.
-- * 'secure' - (Optional) Whether the variable's value will be returned by the 'DescribeApps' action. To conceal an environment variable's value, set @Secure@ to @true@ . @DescribeApps@ then returns @*****FILTERED*****@ instead of the actual value. The default value for @Secure@ is @false@ .
-- * 'value' - (Optional) The environment variable's value, which can be left empty. If you specify a value, it can contain up to 256 characters, which must all be printable.
mkEnvironmentVariable ::
  -- | 'key'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  EnvironmentVariable
mkEnvironmentVariable pKey_ pValue_ =
  EnvironmentVariable'
    { secure = Lude.Nothing,
      key = pKey_,
      value = pValue_
    }

-- | (Optional) Whether the variable's value will be returned by the 'DescribeApps' action. To conceal an environment variable's value, set @Secure@ to @true@ . @DescribeApps@ then returns @*****FILTERED*****@ instead of the actual value. The default value for @Secure@ is @false@ .
--
-- /Note:/ Consider using 'secure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evSecure :: Lens.Lens' EnvironmentVariable (Lude.Maybe Lude.Bool)
evSecure = Lens.lens (secure :: EnvironmentVariable -> Lude.Maybe Lude.Bool) (\s a -> s {secure = a} :: EnvironmentVariable)
{-# DEPRECATED evSecure "Use generic-lens or generic-optics with 'secure' instead." #-}

-- | (Required) The environment variable's name, which can consist of up to 64 characters and must be specified. The name can contain upper- and lowercase letters, numbers, and underscores (_), but it must start with a letter or underscore.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evKey :: Lens.Lens' EnvironmentVariable Lude.Text
evKey = Lens.lens (key :: EnvironmentVariable -> Lude.Text) (\s a -> s {key = a} :: EnvironmentVariable)
{-# DEPRECATED evKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | (Optional) The environment variable's value, which can be left empty. If you specify a value, it can contain up to 256 characters, which must all be printable.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evValue :: Lens.Lens' EnvironmentVariable Lude.Text
evValue = Lens.lens (value :: EnvironmentVariable -> Lude.Text) (\s a -> s {value = a} :: EnvironmentVariable)
{-# DEPRECATED evValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON EnvironmentVariable where
  parseJSON =
    Lude.withObject
      "EnvironmentVariable"
      ( \x ->
          EnvironmentVariable'
            Lude.<$> (x Lude..:? "Secure")
            Lude.<*> (x Lude..: "Key")
            Lude.<*> (x Lude..: "Value")
      )

instance Lude.ToJSON EnvironmentVariable where
  toJSON EnvironmentVariable' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Secure" Lude..=) Lude.<$> secure,
            Lude.Just ("Key" Lude..= key),
            Lude.Just ("Value" Lude..= value)
          ]
      )
