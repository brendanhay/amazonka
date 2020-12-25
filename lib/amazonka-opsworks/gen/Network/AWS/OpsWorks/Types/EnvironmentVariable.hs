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
    evKey,
    evValue,
    evSecure,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.String as Types
import qualified Network.AWS.Prelude as Core

-- | Represents an app's environment variable.
--
-- /See:/ 'mkEnvironmentVariable' smart constructor.
data EnvironmentVariable = EnvironmentVariable'
  { -- | (Required) The environment variable's name, which can consist of up to 64 characters and must be specified. The name can contain upper- and lowercase letters, numbers, and underscores (_), but it must start with a letter or underscore.
    key :: Types.String,
    -- | (Optional) The environment variable's value, which can be left empty. If you specify a value, it can contain up to 256 characters, which must all be printable.
    value :: Types.String,
    -- | (Optional) Whether the variable's value will be returned by the 'DescribeApps' action. To conceal an environment variable's value, set @Secure@ to @true@ . @DescribeApps@ then returns @*****FILTERED*****@ instead of the actual value. The default value for @Secure@ is @false@ .
    secure :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnvironmentVariable' value with any optional fields omitted.
mkEnvironmentVariable ::
  -- | 'key'
  Types.String ->
  -- | 'value'
  Types.String ->
  EnvironmentVariable
mkEnvironmentVariable key value =
  EnvironmentVariable' {key, value, secure = Core.Nothing}

-- | (Required) The environment variable's name, which can consist of up to 64 characters and must be specified. The name can contain upper- and lowercase letters, numbers, and underscores (_), but it must start with a letter or underscore.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evKey :: Lens.Lens' EnvironmentVariable Types.String
evKey = Lens.field @"key"
{-# DEPRECATED evKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | (Optional) The environment variable's value, which can be left empty. If you specify a value, it can contain up to 256 characters, which must all be printable.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evValue :: Lens.Lens' EnvironmentVariable Types.String
evValue = Lens.field @"value"
{-# DEPRECATED evValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | (Optional) Whether the variable's value will be returned by the 'DescribeApps' action. To conceal an environment variable's value, set @Secure@ to @true@ . @DescribeApps@ then returns @*****FILTERED*****@ instead of the actual value. The default value for @Secure@ is @false@ .
--
-- /Note:/ Consider using 'secure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
evSecure :: Lens.Lens' EnvironmentVariable (Core.Maybe Core.Bool)
evSecure = Lens.field @"secure"
{-# DEPRECATED evSecure "Use generic-lens or generic-optics with 'secure' instead." #-}

instance Core.FromJSON EnvironmentVariable where
  toJSON EnvironmentVariable {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Value" Core..= value),
            ("Secure" Core..=) Core.<$> secure
          ]
      )

instance Core.FromJSON EnvironmentVariable where
  parseJSON =
    Core.withObject "EnvironmentVariable" Core.$
      \x ->
        EnvironmentVariable'
          Core.<$> (x Core..: "Key")
          Core.<*> (x Core..: "Value")
          Core.<*> (x Core..:? "Secure")
