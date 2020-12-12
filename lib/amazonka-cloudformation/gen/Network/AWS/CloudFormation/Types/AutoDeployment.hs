{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.AutoDeployment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.AutoDeployment
  ( AutoDeployment (..),

    -- * Smart constructor
    mkAutoDeployment,

    -- * Lenses
    adEnabled,
    adRetainStacksOnAccountRemoval,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | [@Service-managed@ permissions] Describes whether StackSets automatically deploys to AWS Organizations accounts that are added to a target organization or organizational unit (OU).
--
-- /See:/ 'mkAutoDeployment' smart constructor.
data AutoDeployment = AutoDeployment'
  { enabled ::
      Lude.Maybe Lude.Bool,
    retainStacksOnAccountRemoval :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoDeployment' with the minimum fields required to make a request.
--
-- * 'enabled' - If set to @true@ , StackSets automatically deploys additional stack instances to AWS Organizations accounts that are added to a target organization or organizational unit (OU) in the specified Regions. If an account is removed from a target organization or OU, StackSets deletes stack instances from the account in the specified Regions.
-- * 'retainStacksOnAccountRemoval' - If set to @true@ , stack resources are retained when an account is removed from a target organization or OU. If set to @false@ , stack resources are deleted. Specify only if @Enabled@ is set to @True@ .
mkAutoDeployment ::
  AutoDeployment
mkAutoDeployment =
  AutoDeployment'
    { enabled = Lude.Nothing,
      retainStacksOnAccountRemoval = Lude.Nothing
    }

-- | If set to @true@ , StackSets automatically deploys additional stack instances to AWS Organizations accounts that are added to a target organization or organizational unit (OU) in the specified Regions. If an account is removed from a target organization or OU, StackSets deletes stack instances from the account in the specified Regions.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adEnabled :: Lens.Lens' AutoDeployment (Lude.Maybe Lude.Bool)
adEnabled = Lens.lens (enabled :: AutoDeployment -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: AutoDeployment)
{-# DEPRECATED adEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | If set to @true@ , stack resources are retained when an account is removed from a target organization or OU. If set to @false@ , stack resources are deleted. Specify only if @Enabled@ is set to @True@ .
--
-- /Note:/ Consider using 'retainStacksOnAccountRemoval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adRetainStacksOnAccountRemoval :: Lens.Lens' AutoDeployment (Lude.Maybe Lude.Bool)
adRetainStacksOnAccountRemoval = Lens.lens (retainStacksOnAccountRemoval :: AutoDeployment -> Lude.Maybe Lude.Bool) (\s a -> s {retainStacksOnAccountRemoval = a} :: AutoDeployment)
{-# DEPRECATED adRetainStacksOnAccountRemoval "Use generic-lens or generic-optics with 'retainStacksOnAccountRemoval' instead." #-}

instance Lude.FromXML AutoDeployment where
  parseXML x =
    AutoDeployment'
      Lude.<$> (x Lude..@? "Enabled")
      Lude.<*> (x Lude..@? "RetainStacksOnAccountRemoval")

instance Lude.ToQuery AutoDeployment where
  toQuery AutoDeployment' {..} =
    Lude.mconcat
      [ "Enabled" Lude.=: enabled,
        "RetainStacksOnAccountRemoval"
          Lude.=: retainStacksOnAccountRemoval
      ]
