{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.TraceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.TraceSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.AvailabilityZoneDetail
import Network.AWS.XRay.Types.ErrorRootCause
import Network.AWS.XRay.Types.FaultRootCause
import Network.AWS.XRay.Types.Http
import Network.AWS.XRay.Types.InstanceIdDetail
import Network.AWS.XRay.Types.ResourceARNDetail
import Network.AWS.XRay.Types.ResponseTimeRootCause
import Network.AWS.XRay.Types.ServiceId
import Network.AWS.XRay.Types.TraceUser
import Network.AWS.XRay.Types.ValueWithServiceIds

-- | Metadata generated from the segment documents in a trace.
--
-- /See:/ 'newTraceSummary' smart constructor.
data TraceSummary = TraceSummary'
  { -- | A list of EC2 instance IDs for any instance corresponding to the trace
    -- segments.
    instanceIds :: Core.Maybe [InstanceIdDetail],
    -- | A collection of ErrorRootCause structures corresponding to the trace
    -- segments.
    errorRootCauses :: Core.Maybe [ErrorRootCause],
    -- | A list of Availability Zones for any zone corresponding to the trace
    -- segments.
    availabilityZones :: Core.Maybe [AvailabilityZoneDetail],
    -- | The length of time in seconds between the start and end times of the
    -- root segment. If the service performs work asynchronously, the response
    -- time measures the time before the response is sent to the user, while
    -- the duration measures the amount of time before the last traced activity
    -- completes.
    responseTime :: Core.Maybe Core.Double,
    -- | The length of time in seconds between the start time of the root segment
    -- and the end time of the last segment that completed.
    duration :: Core.Maybe Core.Double,
    -- | The matched time stamp of a defined event.
    matchedEventTime :: Core.Maybe Core.POSIX,
    -- | Service IDs from the trace\'s segment documents.
    serviceIds :: Core.Maybe [ServiceId],
    -- | The root segment document has a 500 series error.
    hasFault :: Core.Maybe Core.Bool,
    -- | The root of a trace.
    entryPoint :: Core.Maybe ServiceId,
    -- | The unique identifier for the request that generated the trace\'s
    -- segments and subsegments.
    id :: Core.Maybe Core.Text,
    -- | Annotations from the trace\'s segment documents.
    annotations :: Core.Maybe (Core.HashMap Core.Text [ValueWithServiceIds]),
    -- | A list of resource ARNs for any resource corresponding to the trace
    -- segments.
    resourceARNs :: Core.Maybe [ResourceARNDetail],
    -- | One or more of the segment documents is in progress.
    isPartial :: Core.Maybe Core.Bool,
    -- | A collection of FaultRootCause structures corresponding to the trace
    -- segments.
    faultRootCauses :: Core.Maybe [FaultRootCause],
    -- | The revision number of a trace.
    revision :: Core.Maybe Core.Int,
    -- | Information about the HTTP request served by the trace.
    http :: Core.Maybe Http,
    -- | The root segment document has a 400 series error.
    hasError :: Core.Maybe Core.Bool,
    -- | Users from the trace\'s segment documents.
    users :: Core.Maybe [TraceUser],
    -- | One or more of the segment documents has a 429 throttling error.
    hasThrottle :: Core.Maybe Core.Bool,
    -- | A collection of ResponseTimeRootCause structures corresponding to the
    -- trace segments.
    responseTimeRootCauses :: Core.Maybe [ResponseTimeRootCause]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TraceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'traceSummary_instanceIds' - A list of EC2 instance IDs for any instance corresponding to the trace
-- segments.
--
-- 'errorRootCauses', 'traceSummary_errorRootCauses' - A collection of ErrorRootCause structures corresponding to the trace
-- segments.
--
-- 'availabilityZones', 'traceSummary_availabilityZones' - A list of Availability Zones for any zone corresponding to the trace
-- segments.
--
-- 'responseTime', 'traceSummary_responseTime' - The length of time in seconds between the start and end times of the
-- root segment. If the service performs work asynchronously, the response
-- time measures the time before the response is sent to the user, while
-- the duration measures the amount of time before the last traced activity
-- completes.
--
-- 'duration', 'traceSummary_duration' - The length of time in seconds between the start time of the root segment
-- and the end time of the last segment that completed.
--
-- 'matchedEventTime', 'traceSummary_matchedEventTime' - The matched time stamp of a defined event.
--
-- 'serviceIds', 'traceSummary_serviceIds' - Service IDs from the trace\'s segment documents.
--
-- 'hasFault', 'traceSummary_hasFault' - The root segment document has a 500 series error.
--
-- 'entryPoint', 'traceSummary_entryPoint' - The root of a trace.
--
-- 'id', 'traceSummary_id' - The unique identifier for the request that generated the trace\'s
-- segments and subsegments.
--
-- 'annotations', 'traceSummary_annotations' - Annotations from the trace\'s segment documents.
--
-- 'resourceARNs', 'traceSummary_resourceARNs' - A list of resource ARNs for any resource corresponding to the trace
-- segments.
--
-- 'isPartial', 'traceSummary_isPartial' - One or more of the segment documents is in progress.
--
-- 'faultRootCauses', 'traceSummary_faultRootCauses' - A collection of FaultRootCause structures corresponding to the trace
-- segments.
--
-- 'revision', 'traceSummary_revision' - The revision number of a trace.
--
-- 'http', 'traceSummary_http' - Information about the HTTP request served by the trace.
--
-- 'hasError', 'traceSummary_hasError' - The root segment document has a 400 series error.
--
-- 'users', 'traceSummary_users' - Users from the trace\'s segment documents.
--
-- 'hasThrottle', 'traceSummary_hasThrottle' - One or more of the segment documents has a 429 throttling error.
--
-- 'responseTimeRootCauses', 'traceSummary_responseTimeRootCauses' - A collection of ResponseTimeRootCause structures corresponding to the
-- trace segments.
newTraceSummary ::
  TraceSummary
newTraceSummary =
  TraceSummary'
    { instanceIds = Core.Nothing,
      errorRootCauses = Core.Nothing,
      availabilityZones = Core.Nothing,
      responseTime = Core.Nothing,
      duration = Core.Nothing,
      matchedEventTime = Core.Nothing,
      serviceIds = Core.Nothing,
      hasFault = Core.Nothing,
      entryPoint = Core.Nothing,
      id = Core.Nothing,
      annotations = Core.Nothing,
      resourceARNs = Core.Nothing,
      isPartial = Core.Nothing,
      faultRootCauses = Core.Nothing,
      revision = Core.Nothing,
      http = Core.Nothing,
      hasError = Core.Nothing,
      users = Core.Nothing,
      hasThrottle = Core.Nothing,
      responseTimeRootCauses = Core.Nothing
    }

-- | A list of EC2 instance IDs for any instance corresponding to the trace
-- segments.
traceSummary_instanceIds :: Lens.Lens' TraceSummary (Core.Maybe [InstanceIdDetail])
traceSummary_instanceIds = Lens.lens (\TraceSummary' {instanceIds} -> instanceIds) (\s@TraceSummary' {} a -> s {instanceIds = a} :: TraceSummary) Core.. Lens.mapping Lens._Coerce

-- | A collection of ErrorRootCause structures corresponding to the trace
-- segments.
traceSummary_errorRootCauses :: Lens.Lens' TraceSummary (Core.Maybe [ErrorRootCause])
traceSummary_errorRootCauses = Lens.lens (\TraceSummary' {errorRootCauses} -> errorRootCauses) (\s@TraceSummary' {} a -> s {errorRootCauses = a} :: TraceSummary) Core.. Lens.mapping Lens._Coerce

-- | A list of Availability Zones for any zone corresponding to the trace
-- segments.
traceSummary_availabilityZones :: Lens.Lens' TraceSummary (Core.Maybe [AvailabilityZoneDetail])
traceSummary_availabilityZones = Lens.lens (\TraceSummary' {availabilityZones} -> availabilityZones) (\s@TraceSummary' {} a -> s {availabilityZones = a} :: TraceSummary) Core.. Lens.mapping Lens._Coerce

-- | The length of time in seconds between the start and end times of the
-- root segment. If the service performs work asynchronously, the response
-- time measures the time before the response is sent to the user, while
-- the duration measures the amount of time before the last traced activity
-- completes.
traceSummary_responseTime :: Lens.Lens' TraceSummary (Core.Maybe Core.Double)
traceSummary_responseTime = Lens.lens (\TraceSummary' {responseTime} -> responseTime) (\s@TraceSummary' {} a -> s {responseTime = a} :: TraceSummary)

-- | The length of time in seconds between the start time of the root segment
-- and the end time of the last segment that completed.
traceSummary_duration :: Lens.Lens' TraceSummary (Core.Maybe Core.Double)
traceSummary_duration = Lens.lens (\TraceSummary' {duration} -> duration) (\s@TraceSummary' {} a -> s {duration = a} :: TraceSummary)

-- | The matched time stamp of a defined event.
traceSummary_matchedEventTime :: Lens.Lens' TraceSummary (Core.Maybe Core.UTCTime)
traceSummary_matchedEventTime = Lens.lens (\TraceSummary' {matchedEventTime} -> matchedEventTime) (\s@TraceSummary' {} a -> s {matchedEventTime = a} :: TraceSummary) Core.. Lens.mapping Core._Time

-- | Service IDs from the trace\'s segment documents.
traceSummary_serviceIds :: Lens.Lens' TraceSummary (Core.Maybe [ServiceId])
traceSummary_serviceIds = Lens.lens (\TraceSummary' {serviceIds} -> serviceIds) (\s@TraceSummary' {} a -> s {serviceIds = a} :: TraceSummary) Core.. Lens.mapping Lens._Coerce

-- | The root segment document has a 500 series error.
traceSummary_hasFault :: Lens.Lens' TraceSummary (Core.Maybe Core.Bool)
traceSummary_hasFault = Lens.lens (\TraceSummary' {hasFault} -> hasFault) (\s@TraceSummary' {} a -> s {hasFault = a} :: TraceSummary)

-- | The root of a trace.
traceSummary_entryPoint :: Lens.Lens' TraceSummary (Core.Maybe ServiceId)
traceSummary_entryPoint = Lens.lens (\TraceSummary' {entryPoint} -> entryPoint) (\s@TraceSummary' {} a -> s {entryPoint = a} :: TraceSummary)

-- | The unique identifier for the request that generated the trace\'s
-- segments and subsegments.
traceSummary_id :: Lens.Lens' TraceSummary (Core.Maybe Core.Text)
traceSummary_id = Lens.lens (\TraceSummary' {id} -> id) (\s@TraceSummary' {} a -> s {id = a} :: TraceSummary)

-- | Annotations from the trace\'s segment documents.
traceSummary_annotations :: Lens.Lens' TraceSummary (Core.Maybe (Core.HashMap Core.Text [ValueWithServiceIds]))
traceSummary_annotations = Lens.lens (\TraceSummary' {annotations} -> annotations) (\s@TraceSummary' {} a -> s {annotations = a} :: TraceSummary) Core.. Lens.mapping Lens._Coerce

-- | A list of resource ARNs for any resource corresponding to the trace
-- segments.
traceSummary_resourceARNs :: Lens.Lens' TraceSummary (Core.Maybe [ResourceARNDetail])
traceSummary_resourceARNs = Lens.lens (\TraceSummary' {resourceARNs} -> resourceARNs) (\s@TraceSummary' {} a -> s {resourceARNs = a} :: TraceSummary) Core.. Lens.mapping Lens._Coerce

-- | One or more of the segment documents is in progress.
traceSummary_isPartial :: Lens.Lens' TraceSummary (Core.Maybe Core.Bool)
traceSummary_isPartial = Lens.lens (\TraceSummary' {isPartial} -> isPartial) (\s@TraceSummary' {} a -> s {isPartial = a} :: TraceSummary)

-- | A collection of FaultRootCause structures corresponding to the trace
-- segments.
traceSummary_faultRootCauses :: Lens.Lens' TraceSummary (Core.Maybe [FaultRootCause])
traceSummary_faultRootCauses = Lens.lens (\TraceSummary' {faultRootCauses} -> faultRootCauses) (\s@TraceSummary' {} a -> s {faultRootCauses = a} :: TraceSummary) Core.. Lens.mapping Lens._Coerce

-- | The revision number of a trace.
traceSummary_revision :: Lens.Lens' TraceSummary (Core.Maybe Core.Int)
traceSummary_revision = Lens.lens (\TraceSummary' {revision} -> revision) (\s@TraceSummary' {} a -> s {revision = a} :: TraceSummary)

-- | Information about the HTTP request served by the trace.
traceSummary_http :: Lens.Lens' TraceSummary (Core.Maybe Http)
traceSummary_http = Lens.lens (\TraceSummary' {http} -> http) (\s@TraceSummary' {} a -> s {http = a} :: TraceSummary)

-- | The root segment document has a 400 series error.
traceSummary_hasError :: Lens.Lens' TraceSummary (Core.Maybe Core.Bool)
traceSummary_hasError = Lens.lens (\TraceSummary' {hasError} -> hasError) (\s@TraceSummary' {} a -> s {hasError = a} :: TraceSummary)

-- | Users from the trace\'s segment documents.
traceSummary_users :: Lens.Lens' TraceSummary (Core.Maybe [TraceUser])
traceSummary_users = Lens.lens (\TraceSummary' {users} -> users) (\s@TraceSummary' {} a -> s {users = a} :: TraceSummary) Core.. Lens.mapping Lens._Coerce

-- | One or more of the segment documents has a 429 throttling error.
traceSummary_hasThrottle :: Lens.Lens' TraceSummary (Core.Maybe Core.Bool)
traceSummary_hasThrottle = Lens.lens (\TraceSummary' {hasThrottle} -> hasThrottle) (\s@TraceSummary' {} a -> s {hasThrottle = a} :: TraceSummary)

-- | A collection of ResponseTimeRootCause structures corresponding to the
-- trace segments.
traceSummary_responseTimeRootCauses :: Lens.Lens' TraceSummary (Core.Maybe [ResponseTimeRootCause])
traceSummary_responseTimeRootCauses = Lens.lens (\TraceSummary' {responseTimeRootCauses} -> responseTimeRootCauses) (\s@TraceSummary' {} a -> s {responseTimeRootCauses = a} :: TraceSummary) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON TraceSummary where
  parseJSON =
    Core.withObject
      "TraceSummary"
      ( \x ->
          TraceSummary'
            Core.<$> (x Core..:? "InstanceIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ErrorRootCauses" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AvailabilityZones" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ResponseTime")
            Core.<*> (x Core..:? "Duration")
            Core.<*> (x Core..:? "MatchedEventTime")
            Core.<*> (x Core..:? "ServiceIds" Core..!= Core.mempty)
            Core.<*> (x Core..:? "HasFault")
            Core.<*> (x Core..:? "EntryPoint")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Annotations" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ResourceARNs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "IsPartial")
            Core.<*> (x Core..:? "FaultRootCauses" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Revision")
            Core.<*> (x Core..:? "Http")
            Core.<*> (x Core..:? "HasError")
            Core.<*> (x Core..:? "Users" Core..!= Core.mempty)
            Core.<*> (x Core..:? "HasThrottle")
            Core.<*> ( x Core..:? "ResponseTimeRootCauses"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable TraceSummary

instance Core.NFData TraceSummary
