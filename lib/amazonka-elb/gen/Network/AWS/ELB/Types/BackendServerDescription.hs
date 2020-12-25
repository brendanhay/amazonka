{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.BackendServerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.BackendServerDescription
  ( BackendServerDescription (..),

    -- * Smart constructor
    mkBackendServerDescription,

    -- * Lenses
    bsdInstancePort,
    bsdPolicyNames,
  )
where

import qualified Network.AWS.ELB.Internal as Types
import qualified Network.AWS.ELB.Types.PolicyName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the configuration of an EC2 instance.
--
-- /See:/ 'mkBackendServerDescription' smart constructor.
data BackendServerDescription = BackendServerDescription'
  { -- | The port on which the EC2 instance is listening.
    instancePort :: Core.Maybe Core.Natural,
    -- | The names of the policies enabled for the EC2 instance.
    policyNames :: Core.Maybe [Types.PolicyName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BackendServerDescription' value with any optional fields omitted.
mkBackendServerDescription ::
  BackendServerDescription
mkBackendServerDescription =
  BackendServerDescription'
    { instancePort = Core.Nothing,
      policyNames = Core.Nothing
    }

-- | The port on which the EC2 instance is listening.
--
-- /Note:/ Consider using 'instancePort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsdInstancePort :: Lens.Lens' BackendServerDescription (Core.Maybe Core.Natural)
bsdInstancePort = Lens.field @"instancePort"
{-# DEPRECATED bsdInstancePort "Use generic-lens or generic-optics with 'instancePort' instead." #-}

-- | The names of the policies enabled for the EC2 instance.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsdPolicyNames :: Lens.Lens' BackendServerDescription (Core.Maybe [Types.PolicyName])
bsdPolicyNames = Lens.field @"policyNames"
{-# DEPRECATED bsdPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

instance Core.FromXML BackendServerDescription where
  parseXML x =
    BackendServerDescription'
      Core.<$> (x Core..@? "InstancePort")
      Core.<*> (x Core..@? "PolicyNames" Core..<@> Core.parseXMLList "member")
