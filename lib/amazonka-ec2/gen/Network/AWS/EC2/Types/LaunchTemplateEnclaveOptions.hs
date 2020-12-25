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
import qualified Network.AWS.Prelude as Core

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
-- /See:/ 'mkLaunchTemplateEnclaveOptions' smart constructor.
newtype LaunchTemplateEnclaveOptions = LaunchTemplateEnclaveOptions'
  { -- | If this parameter is set to @true@ , the instance is enabled for AWS Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateEnclaveOptions' value with any optional fields omitted.
mkLaunchTemplateEnclaveOptions ::
  LaunchTemplateEnclaveOptions
mkLaunchTemplateEnclaveOptions =
  LaunchTemplateEnclaveOptions' {enabled = Core.Nothing}

-- | If this parameter is set to @true@ , the instance is enabled for AWS Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lteoEnabled :: Lens.Lens' LaunchTemplateEnclaveOptions (Core.Maybe Core.Bool)
lteoEnabled = Lens.field @"enabled"
{-# DEPRECATED lteoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromXML LaunchTemplateEnclaveOptions where
  parseXML x =
    LaunchTemplateEnclaveOptions' Core.<$> (x Core..@? "enabled")
