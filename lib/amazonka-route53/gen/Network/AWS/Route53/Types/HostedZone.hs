{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53.Types.HostedZone
  ( HostedZone (..)
  -- * Smart constructor
  , mkHostedZone
  -- * Lenses
  , hzId
  , hzName
  , hzCallerReference
  , hzConfig
  , hzLinkedService
  , hzResourceRecordSetCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.CallerReference as Types
import qualified Network.AWS.Route53.Types.HostedZoneConfig as Types
import qualified Network.AWS.Route53.Types.LinkedService as Types
import qualified Network.AWS.Route53.Types.Name as Types

-- | A complex type that contains general information about the hosted zone.
--
-- /See:/ 'mkHostedZone' smart constructor.
data HostedZone = HostedZone'
  { id :: Types.ResourceId
    -- ^ The ID that Amazon Route 53 assigned to the hosted zone when you created it.
  , name :: Types.Name
    -- ^ The name of the domain. For public hosted zones, this is the name that you have registered with your DNS registrar.
--
-- For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone> .
  , callerReference :: Types.CallerReference
    -- ^ The value that you specified for @CallerReference@ when you created the hosted zone.
  , config :: Core.Maybe Types.HostedZoneConfig
    -- ^ A complex type that includes the @Comment@ and @PrivateZone@ elements. If you omitted the @HostedZoneConfig@ and @Comment@ elements from the request, the @Config@ and @Comment@ elements don't appear in the response.
  , linkedService :: Core.Maybe Types.LinkedService
    -- ^ If the hosted zone was created by another service, the service that created the hosted zone. When a hosted zone is created by another service, you can't edit or delete it using Route 53. 
  , resourceRecordSetCount :: Core.Maybe Core.Integer
    -- ^ The number of resource record sets in the hosted zone.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HostedZone' value with any optional fields omitted.
mkHostedZone
    :: Types.ResourceId -- ^ 'id'
    -> Types.Name -- ^ 'name'
    -> Types.CallerReference -- ^ 'callerReference'
    -> HostedZone
mkHostedZone id name callerReference
  = HostedZone'{id, name, callerReference, config = Core.Nothing,
                linkedService = Core.Nothing,
                resourceRecordSetCount = Core.Nothing}

-- | The ID that Amazon Route 53 assigned to the hosted zone when you created it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzId :: Lens.Lens' HostedZone Types.ResourceId
hzId = Lens.field @"id"
{-# INLINEABLE hzId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the domain. For public hosted zones, this is the name that you have registered with your DNS registrar.
--
-- For information about how to specify characters other than @a-z@ , @0-9@ , and @-@ (hyphen) and how to specify internationalized domain names, see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_CreateHostedZone.html CreateHostedZone> .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzName :: Lens.Lens' HostedZone Types.Name
hzName = Lens.field @"name"
{-# INLINEABLE hzName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The value that you specified for @CallerReference@ when you created the hosted zone.
--
-- /Note:/ Consider using 'callerReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzCallerReference :: Lens.Lens' HostedZone Types.CallerReference
hzCallerReference = Lens.field @"callerReference"
{-# INLINEABLE hzCallerReference #-}
{-# DEPRECATED callerReference "Use generic-lens or generic-optics with 'callerReference' instead"  #-}

-- | A complex type that includes the @Comment@ and @PrivateZone@ elements. If you omitted the @HostedZoneConfig@ and @Comment@ elements from the request, the @Config@ and @Comment@ elements don't appear in the response.
--
-- /Note:/ Consider using 'config' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzConfig :: Lens.Lens' HostedZone (Core.Maybe Types.HostedZoneConfig)
hzConfig = Lens.field @"config"
{-# INLINEABLE hzConfig #-}
{-# DEPRECATED config "Use generic-lens or generic-optics with 'config' instead"  #-}

-- | If the hosted zone was created by another service, the service that created the hosted zone. When a hosted zone is created by another service, you can't edit or delete it using Route 53. 
--
-- /Note:/ Consider using 'linkedService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzLinkedService :: Lens.Lens' HostedZone (Core.Maybe Types.LinkedService)
hzLinkedService = Lens.field @"linkedService"
{-# INLINEABLE hzLinkedService #-}
{-# DEPRECATED linkedService "Use generic-lens or generic-optics with 'linkedService' instead"  #-}

-- | The number of resource record sets in the hosted zone.
--
-- /Note:/ Consider using 'resourceRecordSetCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzResourceRecordSetCount :: Lens.Lens' HostedZone (Core.Maybe Core.Integer)
hzResourceRecordSetCount = Lens.field @"resourceRecordSetCount"
{-# INLINEABLE hzResourceRecordSetCount #-}
{-# DEPRECATED resourceRecordSetCount "Use generic-lens or generic-optics with 'resourceRecordSetCount' instead"  #-}

instance Core.FromXML HostedZone where
        parseXML x
          = HostedZone' Core.<$>
              (x Core..@ "Id") Core.<*> x Core..@ "Name" Core.<*>
                x Core..@ "CallerReference"
                Core.<*> x Core..@? "Config"
                Core.<*> x Core..@? "LinkedService"
                Core.<*> x Core..@? "ResourceRecordSetCount"
