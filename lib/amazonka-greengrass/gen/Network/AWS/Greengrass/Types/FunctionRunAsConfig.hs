{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionRunAsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionRunAsConfig
  ( FunctionRunAsConfig (..),

    -- * Smart constructor
    mkFunctionRunAsConfig,

    -- * Lenses
    fracGid,
    fracUid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the user and group whose permissions are used when running the Lambda function. You can specify one or both values to override the default values. We recommend that you avoid running as root unless absolutely necessary to minimize the risk of unintended changes or malicious attacks. To run as root, you must set ''IsolationMode'' to ''NoContainer'' and update config.json in ''greengrass-root/config'' to set ''allowFunctionsToRunAsRoot'' to ''yes''.
--
-- /See:/ 'mkFunctionRunAsConfig' smart constructor.
data FunctionRunAsConfig = FunctionRunAsConfig'
  { -- | The group ID whose permissions are used to run a Lambda function.
    gid :: Core.Maybe Core.Int,
    -- | The user ID whose permissions are used to run a Lambda function.
    uid :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FunctionRunAsConfig' value with any optional fields omitted.
mkFunctionRunAsConfig ::
  FunctionRunAsConfig
mkFunctionRunAsConfig =
  FunctionRunAsConfig' {gid = Core.Nothing, uid = Core.Nothing}

-- | The group ID whose permissions are used to run a Lambda function.
--
-- /Note:/ Consider using 'gid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fracGid :: Lens.Lens' FunctionRunAsConfig (Core.Maybe Core.Int)
fracGid = Lens.field @"gid"
{-# DEPRECATED fracGid "Use generic-lens or generic-optics with 'gid' instead." #-}

-- | The user ID whose permissions are used to run a Lambda function.
--
-- /Note:/ Consider using 'uid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fracUid :: Lens.Lens' FunctionRunAsConfig (Core.Maybe Core.Int)
fracUid = Lens.field @"uid"
{-# DEPRECATED fracUid "Use generic-lens or generic-optics with 'uid' instead." #-}

instance Core.FromJSON FunctionRunAsConfig where
  toJSON FunctionRunAsConfig {..} =
    Core.object
      ( Core.catMaybes
          [("Gid" Core..=) Core.<$> gid, ("Uid" Core..=) Core.<$> uid]
      )

instance Core.FromJSON FunctionRunAsConfig where
  parseJSON =
    Core.withObject "FunctionRunAsConfig" Core.$
      \x ->
        FunctionRunAsConfig'
          Core.<$> (x Core..:? "Gid") Core.<*> (x Core..:? "Uid")
