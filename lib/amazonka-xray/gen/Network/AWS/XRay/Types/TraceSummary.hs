{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TraceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.TraceSummary
  ( TraceSummary (..)
  -- * Smart constructor
  , mkTraceSummary
  -- * Lenses
  , tsAnnotations
  , tsAvailabilityZones
  , tsDuration
  , tsEntryPoint
  , tsErrorRootCauses
  , tsFaultRootCauses
  , tsHasError
  , tsHasFault
  , tsHasThrottle
  , tsHttp
  , tsId
  , tsInstanceIds
  , tsIsPartial
  , tsMatchedEventTime
  , tsResourceARNs
  , tsResponseTime
  , tsResponseTimeRootCauses
  , tsRevision
  , tsServiceIds
  , tsUsers
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.AnnotationKey as Types
import qualified Network.AWS.XRay.Types.AvailabilityZoneDetail as Types
import qualified Network.AWS.XRay.Types.ErrorRootCause as Types
import qualified Network.AWS.XRay.Types.FaultRootCause as Types
import qualified Network.AWS.XRay.Types.Http as Types
import qualified Network.AWS.XRay.Types.InstanceIdDetail as Types
import qualified Network.AWS.XRay.Types.ResourceARNDetail as Types
import qualified Network.AWS.XRay.Types.ResponseTimeRootCause as Types
import qualified Network.AWS.XRay.Types.ServiceId as Types
import qualified Network.AWS.XRay.Types.TraceId as Types
import qualified Network.AWS.XRay.Types.TraceUser as Types
import qualified Network.AWS.XRay.Types.ValueWithServiceIds as Types

-- | Metadata generated from the segment documents in a trace.
--
-- /See:/ 'mkTraceSummary' smart constructor.
data TraceSummary = TraceSummary'
  { annotations :: Core.Maybe (Core.HashMap Types.AnnotationKey [Types.ValueWithServiceIds])
    -- ^ Annotations from the trace's segment documents.
  , availabilityZones :: Core.Maybe [Types.AvailabilityZoneDetail]
    -- ^ A list of Availability Zones for any zone corresponding to the trace segments.
  , duration :: Core.Maybe Core.Double
    -- ^ The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
  , entryPoint :: Core.Maybe Types.ServiceId
    -- ^ The root of a trace.
  , errorRootCauses :: Core.Maybe [Types.ErrorRootCause]
    -- ^ A collection of ErrorRootCause structures corresponding to the trace segments.
  , faultRootCauses :: Core.Maybe [Types.FaultRootCause]
    -- ^ A collection of FaultRootCause structures corresponding to the trace segments.
  , hasError :: Core.Maybe Core.Bool
    -- ^ The root segment document has a 400 series error.
  , hasFault :: Core.Maybe Core.Bool
    -- ^ The root segment document has a 500 series error.
  , hasThrottle :: Core.Maybe Core.Bool
    -- ^ One or more of the segment documents has a 429 throttling error.
  , http :: Core.Maybe Types.Http
    -- ^ Information about the HTTP request served by the trace.
  , id :: Core.Maybe Types.TraceId
    -- ^ The unique identifier for the request that generated the trace's segments and subsegments.
  , instanceIds :: Core.Maybe [Types.InstanceIdDetail]
    -- ^ A list of EC2 instance IDs for any instance corresponding to the trace segments.
  , isPartial :: Core.Maybe Core.Bool
    -- ^ One or more of the segment documents is in progress.
  , matchedEventTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The matched time stamp of a defined event.
  , resourceARNs :: Core.Maybe [Types.ResourceARNDetail]
    -- ^ A list of resource ARNs for any resource corresponding to the trace segments.
  , responseTime :: Core.Maybe Core.Double
    -- ^ The length of time in seconds between the start and end times of the root segment. If the service performs work asynchronously, the response time measures the time before the response is sent to the user, while the duration measures the amount of time before the last traced activity completes.
  , responseTimeRootCauses :: Core.Maybe [Types.ResponseTimeRootCause]
    -- ^ A collection of ResponseTimeRootCause structures corresponding to the trace segments.
  , revision :: Core.Maybe Core.Int
    -- ^ The revision number of a trace.
  , serviceIds :: Core.Maybe [Types.ServiceId]
    -- ^ Service IDs from the trace's segment documents.
  , users :: Core.Maybe [Types.TraceUser]
    -- ^ Users from the trace's segment documents.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TraceSummary' value with any optional fields omitted.
mkTraceSummary
    :: TraceSummary
mkTraceSummary
  = TraceSummary'{annotations = Core.Nothing,
                  availabilityZones = Core.Nothing, duration = Core.Nothing,
                  entryPoint = Core.Nothing, errorRootCauses = Core.Nothing,
                  faultRootCauses = Core.Nothing, hasError = Core.Nothing,
                  hasFault = Core.Nothing, hasThrottle = Core.Nothing,
                  http = Core.Nothing, id = Core.Nothing, instanceIds = Core.Nothing,
                  isPartial = Core.Nothing, matchedEventTime = Core.Nothing,
                  resourceARNs = Core.Nothing, responseTime = Core.Nothing,
                  responseTimeRootCauses = Core.Nothing, revision = Core.Nothing,
                  serviceIds = Core.Nothing, users = Core.Nothing}

-- | Annotations from the trace's segment documents.
--
-- /Note:/ Consider using 'annotations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsAnnotations :: Lens.Lens' TraceSummary (Core.Maybe (Core.HashMap Types.AnnotationKey [Types.ValueWithServiceIds]))
tsAnnotations = Lens.field @"annotations"
{-# INLINEABLE tsAnnotations #-}
{-# DEPRECATED annotations "Use generic-lens or generic-optics with 'annotations' instead"  #-}

-- | A list of Availability Zones for any zone corresponding to the trace segments.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsAvailabilityZones :: Lens.Lens' TraceSummary (Core.Maybe [Types.AvailabilityZoneDetail])
tsAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE tsAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDuration :: Lens.Lens' TraceSummary (Core.Maybe Core.Double)
tsDuration = Lens.field @"duration"
{-# INLINEABLE tsDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | The root of a trace.
--
-- /Note:/ Consider using 'entryPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsEntryPoint :: Lens.Lens' TraceSummary (Core.Maybe Types.ServiceId)
tsEntryPoint = Lens.field @"entryPoint"
{-# INLINEABLE tsEntryPoint #-}
{-# DEPRECATED entryPoint "Use generic-lens or generic-optics with 'entryPoint' instead"  #-}

-- | A collection of ErrorRootCause structures corresponding to the trace segments.
--
-- /Note:/ Consider using 'errorRootCauses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsErrorRootCauses :: Lens.Lens' TraceSummary (Core.Maybe [Types.ErrorRootCause])
tsErrorRootCauses = Lens.field @"errorRootCauses"
{-# INLINEABLE tsErrorRootCauses #-}
{-# DEPRECATED errorRootCauses "Use generic-lens or generic-optics with 'errorRootCauses' instead"  #-}

-- | A collection of FaultRootCause structures corresponding to the trace segments.
--
-- /Note:/ Consider using 'faultRootCauses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFaultRootCauses :: Lens.Lens' TraceSummary (Core.Maybe [Types.FaultRootCause])
tsFaultRootCauses = Lens.field @"faultRootCauses"
{-# INLINEABLE tsFaultRootCauses #-}
{-# DEPRECATED faultRootCauses "Use generic-lens or generic-optics with 'faultRootCauses' instead"  #-}

-- | The root segment document has a 400 series error.
--
-- /Note:/ Consider using 'hasError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsHasError :: Lens.Lens' TraceSummary (Core.Maybe Core.Bool)
tsHasError = Lens.field @"hasError"
{-# INLINEABLE tsHasError #-}
{-# DEPRECATED hasError "Use generic-lens or generic-optics with 'hasError' instead"  #-}

-- | The root segment document has a 500 series error.
--
-- /Note:/ Consider using 'hasFault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsHasFault :: Lens.Lens' TraceSummary (Core.Maybe Core.Bool)
tsHasFault = Lens.field @"hasFault"
{-# INLINEABLE tsHasFault #-}
{-# DEPRECATED hasFault "Use generic-lens or generic-optics with 'hasFault' instead"  #-}

-- | One or more of the segment documents has a 429 throttling error.
--
-- /Note:/ Consider using 'hasThrottle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsHasThrottle :: Lens.Lens' TraceSummary (Core.Maybe Core.Bool)
tsHasThrottle = Lens.field @"hasThrottle"
{-# INLINEABLE tsHasThrottle #-}
{-# DEPRECATED hasThrottle "Use generic-lens or generic-optics with 'hasThrottle' instead"  #-}

-- | Information about the HTTP request served by the trace.
--
-- /Note:/ Consider using 'http' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsHttp :: Lens.Lens' TraceSummary (Core.Maybe Types.Http)
tsHttp = Lens.field @"http"
{-# INLINEABLE tsHttp #-}
{-# DEPRECATED http "Use generic-lens or generic-optics with 'http' instead"  #-}

-- | The unique identifier for the request that generated the trace's segments and subsegments.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsId :: Lens.Lens' TraceSummary (Core.Maybe Types.TraceId)
tsId = Lens.field @"id"
{-# INLINEABLE tsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A list of EC2 instance IDs for any instance corresponding to the trace segments.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsInstanceIds :: Lens.Lens' TraceSummary (Core.Maybe [Types.InstanceIdDetail])
tsInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE tsInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | One or more of the segment documents is in progress.
--
-- /Note:/ Consider using 'isPartial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsIsPartial :: Lens.Lens' TraceSummary (Core.Maybe Core.Bool)
tsIsPartial = Lens.field @"isPartial"
{-# INLINEABLE tsIsPartial #-}
{-# DEPRECATED isPartial "Use generic-lens or generic-optics with 'isPartial' instead"  #-}

-- | The matched time stamp of a defined event.
--
-- /Note:/ Consider using 'matchedEventTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsMatchedEventTime :: Lens.Lens' TraceSummary (Core.Maybe Core.NominalDiffTime)
tsMatchedEventTime = Lens.field @"matchedEventTime"
{-# INLINEABLE tsMatchedEventTime #-}
{-# DEPRECATED matchedEventTime "Use generic-lens or generic-optics with 'matchedEventTime' instead"  #-}

-- | A list of resource ARNs for any resource corresponding to the trace segments.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsResourceARNs :: Lens.Lens' TraceSummary (Core.Maybe [Types.ResourceARNDetail])
tsResourceARNs = Lens.field @"resourceARNs"
{-# INLINEABLE tsResourceARNs #-}
{-# DEPRECATED resourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead"  #-}

-- | The length of time in seconds between the start and end times of the root segment. If the service performs work asynchronously, the response time measures the time before the response is sent to the user, while the duration measures the amount of time before the last traced activity completes.
--
-- /Note:/ Consider using 'responseTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsResponseTime :: Lens.Lens' TraceSummary (Core.Maybe Core.Double)
tsResponseTime = Lens.field @"responseTime"
{-# INLINEABLE tsResponseTime #-}
{-# DEPRECATED responseTime "Use generic-lens or generic-optics with 'responseTime' instead"  #-}

-- | A collection of ResponseTimeRootCause structures corresponding to the trace segments.
--
-- /Note:/ Consider using 'responseTimeRootCauses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsResponseTimeRootCauses :: Lens.Lens' TraceSummary (Core.Maybe [Types.ResponseTimeRootCause])
tsResponseTimeRootCauses = Lens.field @"responseTimeRootCauses"
{-# INLINEABLE tsResponseTimeRootCauses #-}
{-# DEPRECATED responseTimeRootCauses "Use generic-lens or generic-optics with 'responseTimeRootCauses' instead"  #-}

-- | The revision number of a trace.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsRevision :: Lens.Lens' TraceSummary (Core.Maybe Core.Int)
tsRevision = Lens.field @"revision"
{-# INLINEABLE tsRevision #-}
{-# DEPRECATED revision "Use generic-lens or generic-optics with 'revision' instead"  #-}

-- | Service IDs from the trace's segment documents.
--
-- /Note:/ Consider using 'serviceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsServiceIds :: Lens.Lens' TraceSummary (Core.Maybe [Types.ServiceId])
tsServiceIds = Lens.field @"serviceIds"
{-# INLINEABLE tsServiceIds #-}
{-# DEPRECATED serviceIds "Use generic-lens or generic-optics with 'serviceIds' instead"  #-}

-- | Users from the trace's segment documents.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsUsers :: Lens.Lens' TraceSummary (Core.Maybe [Types.TraceUser])
tsUsers = Lens.field @"users"
{-# INLINEABLE tsUsers #-}
{-# DEPRECATED users "Use generic-lens or generic-optics with 'users' instead"  #-}

instance Core.FromJSON TraceSummary where
        parseJSON
          = Core.withObject "TraceSummary" Core.$
              \ x ->
                TraceSummary' Core.<$>
                  (x Core..:? "Annotations") Core.<*> x Core..:? "AvailabilityZones"
                    Core.<*> x Core..:? "Duration"
                    Core.<*> x Core..:? "EntryPoint"
                    Core.<*> x Core..:? "ErrorRootCauses"
                    Core.<*> x Core..:? "FaultRootCauses"
                    Core.<*> x Core..:? "HasError"
                    Core.<*> x Core..:? "HasFault"
                    Core.<*> x Core..:? "HasThrottle"
                    Core.<*> x Core..:? "Http"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "InstanceIds"
                    Core.<*> x Core..:? "IsPartial"
                    Core.<*> x Core..:? "MatchedEventTime"
                    Core.<*> x Core..:? "ResourceARNs"
                    Core.<*> x Core..:? "ResponseTime"
                    Core.<*> x Core..:? "ResponseTimeRootCauses"
                    Core.<*> x Core..:? "Revision"
                    Core.<*> x Core..:? "ServiceIds"
                    Core.<*> x Core..:? "Users"
