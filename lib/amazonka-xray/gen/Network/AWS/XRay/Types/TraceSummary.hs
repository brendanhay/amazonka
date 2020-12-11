-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TraceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TraceSummary
  ( TraceSummary (..),

    -- * Smart constructor
    mkTraceSummary,

    -- * Lenses
    tsAnnotations,
    tsHasThrottle,
    tsUsers,
    tsEntryPoint,
    tsHasFault,
    tsServiceIds,
    tsMatchedEventTime,
    tsIsPartial,
    tsErrorRootCauses,
    tsResourceARNs,
    tsAvailabilityZones,
    tsInstanceIds,
    tsResponseTimeRootCauses,
    tsHasError,
    tsId,
    tsHTTP,
    tsRevision,
    tsDuration,
    tsFaultRootCauses,
    tsResponseTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.AvailabilityZoneDetail
import Network.AWS.XRay.Types.ErrorRootCause
import Network.AWS.XRay.Types.FaultRootCause
import Network.AWS.XRay.Types.HTTP
import Network.AWS.XRay.Types.InstanceIdDetail
import Network.AWS.XRay.Types.ResourceARNDetail
import Network.AWS.XRay.Types.ResponseTimeRootCause
import Network.AWS.XRay.Types.ServiceId
import Network.AWS.XRay.Types.TraceUser
import Network.AWS.XRay.Types.ValueWithServiceIds

-- | Metadata generated from the segment documents in a trace.
--
-- /See:/ 'mkTraceSummary' smart constructor.
data TraceSummary = TraceSummary'
  { annotations ::
      Lude.Maybe (Lude.HashMap Lude.Text ([ValueWithServiceIds])),
    hasThrottle :: Lude.Maybe Lude.Bool,
    users :: Lude.Maybe [TraceUser],
    entryPoint :: Lude.Maybe ServiceId,
    hasFault :: Lude.Maybe Lude.Bool,
    serviceIds :: Lude.Maybe [ServiceId],
    matchedEventTime :: Lude.Maybe Lude.Timestamp,
    isPartial :: Lude.Maybe Lude.Bool,
    errorRootCauses :: Lude.Maybe [ErrorRootCause],
    resourceARNs :: Lude.Maybe [ResourceARNDetail],
    availabilityZones :: Lude.Maybe [AvailabilityZoneDetail],
    instanceIds :: Lude.Maybe [InstanceIdDetail],
    responseTimeRootCauses :: Lude.Maybe [ResponseTimeRootCause],
    hasError :: Lude.Maybe Lude.Bool,
    id :: Lude.Maybe Lude.Text,
    hTTP :: Lude.Maybe HTTP,
    revision :: Lude.Maybe Lude.Int,
    duration :: Lude.Maybe Lude.Double,
    faultRootCauses :: Lude.Maybe [FaultRootCause],
    responseTime :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TraceSummary' with the minimum fields required to make a request.
--
-- * 'annotations' - Annotations from the trace's segment documents.
-- * 'availabilityZones' - A list of Availability Zones for any zone corresponding to the trace segments.
-- * 'duration' - The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
-- * 'entryPoint' - The root of a trace.
-- * 'errorRootCauses' - A collection of ErrorRootCause structures corresponding to the trace segments.
-- * 'faultRootCauses' - A collection of FaultRootCause structures corresponding to the trace segments.
-- * 'hTTP' - Information about the HTTP request served by the trace.
-- * 'hasError' - The root segment document has a 400 series error.
-- * 'hasFault' - The root segment document has a 500 series error.
-- * 'hasThrottle' - One or more of the segment documents has a 429 throttling error.
-- * 'id' - The unique identifier for the request that generated the trace's segments and subsegments.
-- * 'instanceIds' - A list of EC2 instance IDs for any instance corresponding to the trace segments.
-- * 'isPartial' - One or more of the segment documents is in progress.
-- * 'matchedEventTime' - The matched time stamp of a defined event.
-- * 'resourceARNs' - A list of resource ARNs for any resource corresponding to the trace segments.
-- * 'responseTime' - The length of time in seconds between the start and end times of the root segment. If the service performs work asynchronously, the response time measures the time before the response is sent to the user, while the duration measures the amount of time before the last traced activity completes.
-- * 'responseTimeRootCauses' - A collection of ResponseTimeRootCause structures corresponding to the trace segments.
-- * 'revision' - The revision number of a trace.
-- * 'serviceIds' - Service IDs from the trace's segment documents.
-- * 'users' - Users from the trace's segment documents.
mkTraceSummary ::
  TraceSummary
mkTraceSummary =
  TraceSummary'
    { annotations = Lude.Nothing,
      hasThrottle = Lude.Nothing,
      users = Lude.Nothing,
      entryPoint = Lude.Nothing,
      hasFault = Lude.Nothing,
      serviceIds = Lude.Nothing,
      matchedEventTime = Lude.Nothing,
      isPartial = Lude.Nothing,
      errorRootCauses = Lude.Nothing,
      resourceARNs = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      instanceIds = Lude.Nothing,
      responseTimeRootCauses = Lude.Nothing,
      hasError = Lude.Nothing,
      id = Lude.Nothing,
      hTTP = Lude.Nothing,
      revision = Lude.Nothing,
      duration = Lude.Nothing,
      faultRootCauses = Lude.Nothing,
      responseTime = Lude.Nothing
    }

-- | Annotations from the trace's segment documents.
--
-- /Note:/ Consider using 'annotations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsAnnotations :: Lens.Lens' TraceSummary (Lude.Maybe (Lude.HashMap Lude.Text ([ValueWithServiceIds])))
tsAnnotations = Lens.lens (annotations :: TraceSummary -> Lude.Maybe (Lude.HashMap Lude.Text ([ValueWithServiceIds]))) (\s a -> s {annotations = a} :: TraceSummary)
{-# DEPRECATED tsAnnotations "Use generic-lens or generic-optics with 'annotations' instead." #-}

-- | One or more of the segment documents has a 429 throttling error.
--
-- /Note:/ Consider using 'hasThrottle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsHasThrottle :: Lens.Lens' TraceSummary (Lude.Maybe Lude.Bool)
tsHasThrottle = Lens.lens (hasThrottle :: TraceSummary -> Lude.Maybe Lude.Bool) (\s a -> s {hasThrottle = a} :: TraceSummary)
{-# DEPRECATED tsHasThrottle "Use generic-lens or generic-optics with 'hasThrottle' instead." #-}

-- | Users from the trace's segment documents.
--
-- /Note:/ Consider using 'users' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsUsers :: Lens.Lens' TraceSummary (Lude.Maybe [TraceUser])
tsUsers = Lens.lens (users :: TraceSummary -> Lude.Maybe [TraceUser]) (\s a -> s {users = a} :: TraceSummary)
{-# DEPRECATED tsUsers "Use generic-lens or generic-optics with 'users' instead." #-}

-- | The root of a trace.
--
-- /Note:/ Consider using 'entryPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsEntryPoint :: Lens.Lens' TraceSummary (Lude.Maybe ServiceId)
tsEntryPoint = Lens.lens (entryPoint :: TraceSummary -> Lude.Maybe ServiceId) (\s a -> s {entryPoint = a} :: TraceSummary)
{-# DEPRECATED tsEntryPoint "Use generic-lens or generic-optics with 'entryPoint' instead." #-}

-- | The root segment document has a 500 series error.
--
-- /Note:/ Consider using 'hasFault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsHasFault :: Lens.Lens' TraceSummary (Lude.Maybe Lude.Bool)
tsHasFault = Lens.lens (hasFault :: TraceSummary -> Lude.Maybe Lude.Bool) (\s a -> s {hasFault = a} :: TraceSummary)
{-# DEPRECATED tsHasFault "Use generic-lens or generic-optics with 'hasFault' instead." #-}

-- | Service IDs from the trace's segment documents.
--
-- /Note:/ Consider using 'serviceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsServiceIds :: Lens.Lens' TraceSummary (Lude.Maybe [ServiceId])
tsServiceIds = Lens.lens (serviceIds :: TraceSummary -> Lude.Maybe [ServiceId]) (\s a -> s {serviceIds = a} :: TraceSummary)
{-# DEPRECATED tsServiceIds "Use generic-lens or generic-optics with 'serviceIds' instead." #-}

-- | The matched time stamp of a defined event.
--
-- /Note:/ Consider using 'matchedEventTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsMatchedEventTime :: Lens.Lens' TraceSummary (Lude.Maybe Lude.Timestamp)
tsMatchedEventTime = Lens.lens (matchedEventTime :: TraceSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {matchedEventTime = a} :: TraceSummary)
{-# DEPRECATED tsMatchedEventTime "Use generic-lens or generic-optics with 'matchedEventTime' instead." #-}

-- | One or more of the segment documents is in progress.
--
-- /Note:/ Consider using 'isPartial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsIsPartial :: Lens.Lens' TraceSummary (Lude.Maybe Lude.Bool)
tsIsPartial = Lens.lens (isPartial :: TraceSummary -> Lude.Maybe Lude.Bool) (\s a -> s {isPartial = a} :: TraceSummary)
{-# DEPRECATED tsIsPartial "Use generic-lens or generic-optics with 'isPartial' instead." #-}

-- | A collection of ErrorRootCause structures corresponding to the trace segments.
--
-- /Note:/ Consider using 'errorRootCauses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsErrorRootCauses :: Lens.Lens' TraceSummary (Lude.Maybe [ErrorRootCause])
tsErrorRootCauses = Lens.lens (errorRootCauses :: TraceSummary -> Lude.Maybe [ErrorRootCause]) (\s a -> s {errorRootCauses = a} :: TraceSummary)
{-# DEPRECATED tsErrorRootCauses "Use generic-lens or generic-optics with 'errorRootCauses' instead." #-}

-- | A list of resource ARNs for any resource corresponding to the trace segments.
--
-- /Note:/ Consider using 'resourceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsResourceARNs :: Lens.Lens' TraceSummary (Lude.Maybe [ResourceARNDetail])
tsResourceARNs = Lens.lens (resourceARNs :: TraceSummary -> Lude.Maybe [ResourceARNDetail]) (\s a -> s {resourceARNs = a} :: TraceSummary)
{-# DEPRECATED tsResourceARNs "Use generic-lens or generic-optics with 'resourceARNs' instead." #-}

-- | A list of Availability Zones for any zone corresponding to the trace segments.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsAvailabilityZones :: Lens.Lens' TraceSummary (Lude.Maybe [AvailabilityZoneDetail])
tsAvailabilityZones = Lens.lens (availabilityZones :: TraceSummary -> Lude.Maybe [AvailabilityZoneDetail]) (\s a -> s {availabilityZones = a} :: TraceSummary)
{-# DEPRECATED tsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | A list of EC2 instance IDs for any instance corresponding to the trace segments.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsInstanceIds :: Lens.Lens' TraceSummary (Lude.Maybe [InstanceIdDetail])
tsInstanceIds = Lens.lens (instanceIds :: TraceSummary -> Lude.Maybe [InstanceIdDetail]) (\s a -> s {instanceIds = a} :: TraceSummary)
{-# DEPRECATED tsInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | A collection of ResponseTimeRootCause structures corresponding to the trace segments.
--
-- /Note:/ Consider using 'responseTimeRootCauses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsResponseTimeRootCauses :: Lens.Lens' TraceSummary (Lude.Maybe [ResponseTimeRootCause])
tsResponseTimeRootCauses = Lens.lens (responseTimeRootCauses :: TraceSummary -> Lude.Maybe [ResponseTimeRootCause]) (\s a -> s {responseTimeRootCauses = a} :: TraceSummary)
{-# DEPRECATED tsResponseTimeRootCauses "Use generic-lens or generic-optics with 'responseTimeRootCauses' instead." #-}

-- | The root segment document has a 400 series error.
--
-- /Note:/ Consider using 'hasError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsHasError :: Lens.Lens' TraceSummary (Lude.Maybe Lude.Bool)
tsHasError = Lens.lens (hasError :: TraceSummary -> Lude.Maybe Lude.Bool) (\s a -> s {hasError = a} :: TraceSummary)
{-# DEPRECATED tsHasError "Use generic-lens or generic-optics with 'hasError' instead." #-}

-- | The unique identifier for the request that generated the trace's segments and subsegments.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsId :: Lens.Lens' TraceSummary (Lude.Maybe Lude.Text)
tsId = Lens.lens (id :: TraceSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: TraceSummary)
{-# DEPRECATED tsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Information about the HTTP request served by the trace.
--
-- /Note:/ Consider using 'hTTP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsHTTP :: Lens.Lens' TraceSummary (Lude.Maybe HTTP)
tsHTTP = Lens.lens (hTTP :: TraceSummary -> Lude.Maybe HTTP) (\s a -> s {hTTP = a} :: TraceSummary)
{-# DEPRECATED tsHTTP "Use generic-lens or generic-optics with 'hTTP' instead." #-}

-- | The revision number of a trace.
--
-- /Note:/ Consider using 'revision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsRevision :: Lens.Lens' TraceSummary (Lude.Maybe Lude.Int)
tsRevision = Lens.lens (revision :: TraceSummary -> Lude.Maybe Lude.Int) (\s a -> s {revision = a} :: TraceSummary)
{-# DEPRECATED tsRevision "Use generic-lens or generic-optics with 'revision' instead." #-}

-- | The length of time in seconds between the start time of the root segment and the end time of the last segment that completed.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsDuration :: Lens.Lens' TraceSummary (Lude.Maybe Lude.Double)
tsDuration = Lens.lens (duration :: TraceSummary -> Lude.Maybe Lude.Double) (\s a -> s {duration = a} :: TraceSummary)
{-# DEPRECATED tsDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | A collection of FaultRootCause structures corresponding to the trace segments.
--
-- /Note:/ Consider using 'faultRootCauses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsFaultRootCauses :: Lens.Lens' TraceSummary (Lude.Maybe [FaultRootCause])
tsFaultRootCauses = Lens.lens (faultRootCauses :: TraceSummary -> Lude.Maybe [FaultRootCause]) (\s a -> s {faultRootCauses = a} :: TraceSummary)
{-# DEPRECATED tsFaultRootCauses "Use generic-lens or generic-optics with 'faultRootCauses' instead." #-}

-- | The length of time in seconds between the start and end times of the root segment. If the service performs work asynchronously, the response time measures the time before the response is sent to the user, while the duration measures the amount of time before the last traced activity completes.
--
-- /Note:/ Consider using 'responseTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsResponseTime :: Lens.Lens' TraceSummary (Lude.Maybe Lude.Double)
tsResponseTime = Lens.lens (responseTime :: TraceSummary -> Lude.Maybe Lude.Double) (\s a -> s {responseTime = a} :: TraceSummary)
{-# DEPRECATED tsResponseTime "Use generic-lens or generic-optics with 'responseTime' instead." #-}

instance Lude.FromJSON TraceSummary where
  parseJSON =
    Lude.withObject
      "TraceSummary"
      ( \x ->
          TraceSummary'
            Lude.<$> (x Lude..:? "Annotations" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "HasThrottle")
            Lude.<*> (x Lude..:? "Users" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EntryPoint")
            Lude.<*> (x Lude..:? "HasFault")
            Lude.<*> (x Lude..:? "ServiceIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "MatchedEventTime")
            Lude.<*> (x Lude..:? "IsPartial")
            Lude.<*> (x Lude..:? "ErrorRootCauses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResourceARNs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AvailabilityZones" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "InstanceIds" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResponseTimeRootCauses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "HasError")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Http")
            Lude.<*> (x Lude..:? "Revision")
            Lude.<*> (x Lude..:? "Duration")
            Lude.<*> (x Lude..:? "FaultRootCauses" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResponseTime")
      )
