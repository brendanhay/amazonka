{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.DelegationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.DelegationSet
  ( DelegationSet (..),

    -- * Smart constructor
    mkDelegationSet,

    -- * Lenses
    dsNameServers,
    dsCallerReference,
    dsId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.CallerReference as Types
import qualified Network.AWS.Route53.Types.DNSName as Types

-- | A complex type that lists the name servers in a delegation set, as well as the @CallerReference@ and the @ID@ for the delegation set.
--
-- /See:/ 'mkDelegationSet' smart constructor.
data DelegationSet = DelegationSet'
  { -- | A complex type that contains a list of the authoritative name servers for a hosted zone or for a reusable delegation set.
    nameServers :: Core.NonEmpty Types.DNSName,
    -- | The value that you specified for @CallerReference@ when you created the reusable delegation set.
    callerReference :: Core.Maybe Types.CallerReference,
    -- | The ID that Amazon Route 53 assigns to a reusable delegation set.
    id :: Core.Maybe Types.ResourceId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DelegationSet' value with any optional fields omitted.
mkDelegationSet ::
  -- | 'nameServers'
  Core.NonEmpty Types.DNSName ->
  DelegationSet
mkDelegationSet nameServers =
  DelegationSet'
    { nameServers,
      callerReference = Core.Nothing,
      id = Core.Nothing
    }

-- | A complex type that contains a list of the authoritative name servers for a hosted zone or for a reusable delegation set.
--
-- /Note:/ Consider using 'nameServers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsNameServers :: Lens.Lens' DelegationSet (Core.NonEmpty Types.DNSName)
dsNameServers = Lens.field @"nameServers"
{-# DEPRECATED dsNameServers "Use generic-lens or generic-optics with 'nameServers' instead." #-}

-- | The value that you specified for @CallerReference@ when you created the reusable delegation set.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCallerReference :: Lens.Lens' DelegationSet (Core.Maybe Types.CallerReference)
dsCallerReference = Lens.field @"callerReference"
{-# DEPRECATED dsCallerReference "Use generic-lens or generic-optics with 'callerReference' instead." #-}

-- | The ID that Amazon Route 53 assigns to a reusable delegation set.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsId :: Lens.Lens' DelegationSet (Core.Maybe Types.ResourceId)
dsId = Lens.field @"id"
{-# DEPRECATED dsId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromXML DelegationSet where
  parseXML x =
    DelegationSet'
      Core.<$> ( x Core..@ "NameServers"
                   Core..<@> Core.parseXMLNonEmpty "NameServer"
               )
      Core.<*> (x Core..@? "CallerReference")
      Core.<*> (x Core..@? "Id")
