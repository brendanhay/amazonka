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
    fracUid,
    fracGid,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the user and group whose permissions are used when running the Lambda function. You can specify one or both values to override the default values. We recommend that you avoid running as root unless absolutely necessary to minimize the risk of unintended changes or malicious attacks. To run as root, you must set ''IsolationMode'' to ''NoContainer'' and update config.json in ''greengrass-root/config'' to set ''allowFunctionsToRunAsRoot'' to ''yes''.
--
-- /See:/ 'mkFunctionRunAsConfig' smart constructor.
data FunctionRunAsConfig = FunctionRunAsConfig'
  { -- | The user ID whose permissions are used to run a Lambda function.
    uid :: Lude.Maybe Lude.Int,
    -- | The group ID whose permissions are used to run a Lambda function.
    gid :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FunctionRunAsConfig' with the minimum fields required to make a request.
--
-- * 'uid' - The user ID whose permissions are used to run a Lambda function.
-- * 'gid' - The group ID whose permissions are used to run a Lambda function.
mkFunctionRunAsConfig ::
  FunctionRunAsConfig
mkFunctionRunAsConfig =
  FunctionRunAsConfig' {uid = Lude.Nothing, gid = Lude.Nothing}

-- | The user ID whose permissions are used to run a Lambda function.
--
-- /Note:/ Consider using 'uid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fracUid :: Lens.Lens' FunctionRunAsConfig (Lude.Maybe Lude.Int)
fracUid = Lens.lens (uid :: FunctionRunAsConfig -> Lude.Maybe Lude.Int) (\s a -> s {uid = a} :: FunctionRunAsConfig)
{-# DEPRECATED fracUid "Use generic-lens or generic-optics with 'uid' instead." #-}

-- | The group ID whose permissions are used to run a Lambda function.
--
-- /Note:/ Consider using 'gid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fracGid :: Lens.Lens' FunctionRunAsConfig (Lude.Maybe Lude.Int)
fracGid = Lens.lens (gid :: FunctionRunAsConfig -> Lude.Maybe Lude.Int) (\s a -> s {gid = a} :: FunctionRunAsConfig)
{-# DEPRECATED fracGid "Use generic-lens or generic-optics with 'gid' instead." #-}

instance Lude.FromJSON FunctionRunAsConfig where
  parseJSON =
    Lude.withObject
      "FunctionRunAsConfig"
      ( \x ->
          FunctionRunAsConfig'
            Lude.<$> (x Lude..:? "Uid") Lude.<*> (x Lude..:? "Gid")
      )

instance Lude.ToJSON FunctionRunAsConfig where
  toJSON FunctionRunAsConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Uid" Lude..=) Lude.<$> uid, ("Gid" Lude..=) Lude.<$> gid]
      )
