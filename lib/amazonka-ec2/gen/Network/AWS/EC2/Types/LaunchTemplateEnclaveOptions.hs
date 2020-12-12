{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEnclaveOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateEnclaveOptions
  ( LaunchTemplateEnclaveOptions (..),

    -- * Smart constructor
    mkLaunchTemplateEnclaveOptions,

    -- * Lenses
    lteoEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /See:/ 'mkLaunchTemplateEnclaveOptions' smart constructor.
newtype LaunchTemplateEnclaveOptions = LaunchTemplateEnclaveOptions'
  { enabled ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateEnclaveOptions' with the minimum fields required to make a request.
--
-- * 'enabled' - If this parameter is set to @true@ , the instance is enabled for AWS Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
mkLaunchTemplateEnclaveOptions ::
  LaunchTemplateEnclaveOptions
mkLaunchTemplateEnclaveOptions =
  LaunchTemplateEnclaveOptions' {enabled = Lude.Nothing}

-- | If this parameter is set to @true@ , the instance is enabled for AWS Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteoEnabled :: Lens.Lens' LaunchTemplateEnclaveOptions (Lude.Maybe Lude.Bool)
lteoEnabled = Lens.lens (enabled :: LaunchTemplateEnclaveOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: LaunchTemplateEnclaveOptions)
{-# DEPRECATED lteoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Lude.FromXML LaunchTemplateEnclaveOptions where
  parseXML x =
    LaunchTemplateEnclaveOptions' Lude.<$> (x Lude..@? "enabled")
