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
-- Module      : Amazonka.XRay.Types.TraceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.TraceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.AvailabilityZoneDetail
import Amazonka.XRay.Types.ErrorRootCause
import Amazonka.XRay.Types.FaultRootCause
import Amazonka.XRay.Types.Http
import Amazonka.XRay.Types.InstanceIdDetail
import Amazonka.XRay.Types.ResourceARNDetail
import Amazonka.XRay.Types.ResponseTimeRootCause
import Amazonka.XRay.Types.ServiceId
import Amazonka.XRay.Types.TraceUser
import Amazonka.XRay.Types.ValueWithServiceIds

-- | Metadata generated from the segment documents in a trace.
--
-- /See:/ 'newTraceSummary' smart constructor.
data TraceSummary = TraceSummary'
  { -- | Information about the HTTP request served by the trace.
    http :: Prelude.Maybe Http,
    -- | A collection of ResponseTimeRootCause structures corresponding to the
    -- trace segments.
    responseTimeRootCauses :: Prelude.Maybe [ResponseTimeRootCause],
    -- | The root segment document has a 500 series error.
    hasFault :: Prelude.Maybe Prelude.Bool,
    -- | One or more of the segment documents is in progress.
    isPartial :: Prelude.Maybe Prelude.Bool,
    -- | A list of Availability Zones for any zone corresponding to the trace
    -- segments.
    availabilityZones :: Prelude.Maybe [AvailabilityZoneDetail],
    -- | The matched time stamp of a defined event.
    matchedEventTime :: Prelude.Maybe Core.POSIX,
    -- | The revision number of a trace.
    revision :: Prelude.Maybe Prelude.Int,
    -- | A collection of ErrorRootCause structures corresponding to the trace
    -- segments.
    errorRootCauses :: Prelude.Maybe [ErrorRootCause],
    -- | Users from the trace\'s segment documents.
    users :: Prelude.Maybe [TraceUser],
    -- | The root segment document has a 400 series error.
    hasError :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the request that generated the trace\'s
    -- segments and subsegments.
    id :: Prelude.Maybe Prelude.Text,
    -- | Annotations from the trace\'s segment documents.
    annotations :: Prelude.Maybe (Prelude.HashMap Prelude.Text [ValueWithServiceIds]),
    -- | The length of time in seconds between the start time of the root segment
    -- and the end time of the last segment that completed.
    duration :: Prelude.Maybe Prelude.Double,
    -- | One or more of the segment documents has a 429 throttling error.
    hasThrottle :: Prelude.Maybe Prelude.Bool,
    -- | A collection of FaultRootCause structures corresponding to the trace
    -- segments.
    faultRootCauses :: Prelude.Maybe [FaultRootCause],
    -- | Service IDs from the trace\'s segment documents.
    serviceIds :: Prelude.Maybe [ServiceId],
    -- | The length of time in seconds between the start and end times of the
    -- root segment. If the service performs work asynchronously, the response
    -- time measures the time before the response is sent to the user, while
    -- the duration measures the amount of time before the last traced activity
    -- completes.
    responseTime :: Prelude.Maybe Prelude.Double,
    -- | The root of a trace.
    entryPoint :: Prelude.Maybe ServiceId,
    -- | A list of EC2 instance IDs for any instance corresponding to the trace
    -- segments.
    instanceIds :: Prelude.Maybe [InstanceIdDetail],
    -- | A list of resource ARNs for any resource corresponding to the trace
    -- segments.
    resourceARNs :: Prelude.Maybe [ResourceARNDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TraceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'http', 'traceSummary_http' - Information about the HTTP request served by the trace.
--
-- 'responseTimeRootCauses', 'traceSummary_responseTimeRootCauses' - A collection of ResponseTimeRootCause structures corresponding to the
-- trace segments.
--
-- 'hasFault', 'traceSummary_hasFault' - The root segment document has a 500 series error.
--
-- 'isPartial', 'traceSummary_isPartial' - One or more of the segment documents is in progress.
--
-- 'availabilityZones', 'traceSummary_availabilityZones' - A list of Availability Zones for any zone corresponding to the trace
-- segments.
--
-- 'matchedEventTime', 'traceSummary_matchedEventTime' - The matched time stamp of a defined event.
--
-- 'revision', 'traceSummary_revision' - The revision number of a trace.
--
-- 'errorRootCauses', 'traceSummary_errorRootCauses' - A collection of ErrorRootCause structures corresponding to the trace
-- segments.
--
-- 'users', 'traceSummary_users' - Users from the trace\'s segment documents.
--
-- 'hasError', 'traceSummary_hasError' - The root segment document has a 400 series error.
--
-- 'id', 'traceSummary_id' - The unique identifier for the request that generated the trace\'s
-- segments and subsegments.
--
-- 'annotations', 'traceSummary_annotations' - Annotations from the trace\'s segment documents.
--
-- 'duration', 'traceSummary_duration' - The length of time in seconds between the start time of the root segment
-- and the end time of the last segment that completed.
--
-- 'hasThrottle', 'traceSummary_hasThrottle' - One or more of the segment documents has a 429 throttling error.
--
-- 'faultRootCauses', 'traceSummary_faultRootCauses' - A collection of FaultRootCause structures corresponding to the trace
-- segments.
--
-- 'serviceIds', 'traceSummary_serviceIds' - Service IDs from the trace\'s segment documents.
--
-- 'responseTime', 'traceSummary_responseTime' - The length of time in seconds between the start and end times of the
-- root segment. If the service performs work asynchronously, the response
-- time measures the time before the response is sent to the user, while
-- the duration measures the amount of time before the last traced activity
-- completes.
--
-- 'entryPoint', 'traceSummary_entryPoint' - The root of a trace.
--
-- 'instanceIds', 'traceSummary_instanceIds' - A list of EC2 instance IDs for any instance corresponding to the trace
-- segments.
--
-- 'resourceARNs', 'traceSummary_resourceARNs' - A list of resource ARNs for any resource corresponding to the trace
-- segments.
newTraceSummary ::
  TraceSummary
newTraceSummary =
  TraceSummary'
    { http = Prelude.Nothing,
      responseTimeRootCauses = Prelude.Nothing,
      hasFault = Prelude.Nothing,
      isPartial = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      matchedEventTime = Prelude.Nothing,
      revision = Prelude.Nothing,
      errorRootCauses = Prelude.Nothing,
      users = Prelude.Nothing,
      hasError = Prelude.Nothing,
      id = Prelude.Nothing,
      annotations = Prelude.Nothing,
      duration = Prelude.Nothing,
      hasThrottle = Prelude.Nothing,
      faultRootCauses = Prelude.Nothing,
      serviceIds = Prelude.Nothing,
      responseTime = Prelude.Nothing,
      entryPoint = Prelude.Nothing,
      instanceIds = Prelude.Nothing,
      resourceARNs = Prelude.Nothing
    }

-- | Information about the HTTP request served by the trace.
traceSummary_http :: Lens.Lens' TraceSummary (Prelude.Maybe Http)
traceSummary_http = Lens.lens (\TraceSummary' {http} -> http) (\s@TraceSummary' {} a -> s {http = a} :: TraceSummary)

-- | A collection of ResponseTimeRootCause structures corresponding to the
-- trace segments.
traceSummary_responseTimeRootCauses :: Lens.Lens' TraceSummary (Prelude.Maybe [ResponseTimeRootCause])
traceSummary_responseTimeRootCauses = Lens.lens (\TraceSummary' {responseTimeRootCauses} -> responseTimeRootCauses) (\s@TraceSummary' {} a -> s {responseTimeRootCauses = a} :: TraceSummary) Prelude.. Lens.mapping Lens.coerced

-- | The root segment document has a 500 series error.
traceSummary_hasFault :: Lens.Lens' TraceSummary (Prelude.Maybe Prelude.Bool)
traceSummary_hasFault = Lens.lens (\TraceSummary' {hasFault} -> hasFault) (\s@TraceSummary' {} a -> s {hasFault = a} :: TraceSummary)

-- | One or more of the segment documents is in progress.
traceSummary_isPartial :: Lens.Lens' TraceSummary (Prelude.Maybe Prelude.Bool)
traceSummary_isPartial = Lens.lens (\TraceSummary' {isPartial} -> isPartial) (\s@TraceSummary' {} a -> s {isPartial = a} :: TraceSummary)

-- | A list of Availability Zones for any zone corresponding to the trace
-- segments.
traceSummary_availabilityZones :: Lens.Lens' TraceSummary (Prelude.Maybe [AvailabilityZoneDetail])
traceSummary_availabilityZones = Lens.lens (\TraceSummary' {availabilityZones} -> availabilityZones) (\s@TraceSummary' {} a -> s {availabilityZones = a} :: TraceSummary) Prelude.. Lens.mapping Lens.coerced

-- | The matched time stamp of a defined event.
traceSummary_matchedEventTime :: Lens.Lens' TraceSummary (Prelude.Maybe Prelude.UTCTime)
traceSummary_matchedEventTime = Lens.lens (\TraceSummary' {matchedEventTime} -> matchedEventTime) (\s@TraceSummary' {} a -> s {matchedEventTime = a} :: TraceSummary) Prelude.. Lens.mapping Core._Time

-- | The revision number of a trace.
traceSummary_revision :: Lens.Lens' TraceSummary (Prelude.Maybe Prelude.Int)
traceSummary_revision = Lens.lens (\TraceSummary' {revision} -> revision) (\s@TraceSummary' {} a -> s {revision = a} :: TraceSummary)

-- | A collection of ErrorRootCause structures corresponding to the trace
-- segments.
traceSummary_errorRootCauses :: Lens.Lens' TraceSummary (Prelude.Maybe [ErrorRootCause])
traceSummary_errorRootCauses = Lens.lens (\TraceSummary' {errorRootCauses} -> errorRootCauses) (\s@TraceSummary' {} a -> s {errorRootCauses = a} :: TraceSummary) Prelude.. Lens.mapping Lens.coerced

-- | Users from the trace\'s segment documents.
traceSummary_users :: Lens.Lens' TraceSummary (Prelude.Maybe [TraceUser])
traceSummary_users = Lens.lens (\TraceSummary' {users} -> users) (\s@TraceSummary' {} a -> s {users = a} :: TraceSummary) Prelude.. Lens.mapping Lens.coerced

-- | The root segment document has a 400 series error.
traceSummary_hasError :: Lens.Lens' TraceSummary (Prelude.Maybe Prelude.Bool)
traceSummary_hasError = Lens.lens (\TraceSummary' {hasError} -> hasError) (\s@TraceSummary' {} a -> s {hasError = a} :: TraceSummary)

-- | The unique identifier for the request that generated the trace\'s
-- segments and subsegments.
traceSummary_id :: Lens.Lens' TraceSummary (Prelude.Maybe Prelude.Text)
traceSummary_id = Lens.lens (\TraceSummary' {id} -> id) (\s@TraceSummary' {} a -> s {id = a} :: TraceSummary)

-- | Annotations from the trace\'s segment documents.
traceSummary_annotations :: Lens.Lens' TraceSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text [ValueWithServiceIds]))
traceSummary_annotations = Lens.lens (\TraceSummary' {annotations} -> annotations) (\s@TraceSummary' {} a -> s {annotations = a} :: TraceSummary) Prelude.. Lens.mapping Lens.coerced

-- | The length of time in seconds between the start time of the root segment
-- and the end time of the last segment that completed.
traceSummary_duration :: Lens.Lens' TraceSummary (Prelude.Maybe Prelude.Double)
traceSummary_duration = Lens.lens (\TraceSummary' {duration} -> duration) (\s@TraceSummary' {} a -> s {duration = a} :: TraceSummary)

-- | One or more of the segment documents has a 429 throttling error.
traceSummary_hasThrottle :: Lens.Lens' TraceSummary (Prelude.Maybe Prelude.Bool)
traceSummary_hasThrottle = Lens.lens (\TraceSummary' {hasThrottle} -> hasThrottle) (\s@TraceSummary' {} a -> s {hasThrottle = a} :: TraceSummary)

-- | A collection of FaultRootCause structures corresponding to the trace
-- segments.
traceSummary_faultRootCauses :: Lens.Lens' TraceSummary (Prelude.Maybe [FaultRootCause])
traceSummary_faultRootCauses = Lens.lens (\TraceSummary' {faultRootCauses} -> faultRootCauses) (\s@TraceSummary' {} a -> s {faultRootCauses = a} :: TraceSummary) Prelude.. Lens.mapping Lens.coerced

-- | Service IDs from the trace\'s segment documents.
traceSummary_serviceIds :: Lens.Lens' TraceSummary (Prelude.Maybe [ServiceId])
traceSummary_serviceIds = Lens.lens (\TraceSummary' {serviceIds} -> serviceIds) (\s@TraceSummary' {} a -> s {serviceIds = a} :: TraceSummary) Prelude.. Lens.mapping Lens.coerced

-- | The length of time in seconds between the start and end times of the
-- root segment. If the service performs work asynchronously, the response
-- time measures the time before the response is sent to the user, while
-- the duration measures the amount of time before the last traced activity
-- completes.
traceSummary_responseTime :: Lens.Lens' TraceSummary (Prelude.Maybe Prelude.Double)
traceSummary_responseTime = Lens.lens (\TraceSummary' {responseTime} -> responseTime) (\s@TraceSummary' {} a -> s {responseTime = a} :: TraceSummary)

-- | The root of a trace.
traceSummary_entryPoint :: Lens.Lens' TraceSummary (Prelude.Maybe ServiceId)
traceSummary_entryPoint = Lens.lens (\TraceSummary' {entryPoint} -> entryPoint) (\s@TraceSummary' {} a -> s {entryPoint = a} :: TraceSummary)

-- | A list of EC2 instance IDs for any instance corresponding to the trace
-- segments.
traceSummary_instanceIds :: Lens.Lens' TraceSummary (Prelude.Maybe [InstanceIdDetail])
traceSummary_instanceIds = Lens.lens (\TraceSummary' {instanceIds} -> instanceIds) (\s@TraceSummary' {} a -> s {instanceIds = a} :: TraceSummary) Prelude.. Lens.mapping Lens.coerced

-- | A list of resource ARNs for any resource corresponding to the trace
-- segments.
traceSummary_resourceARNs :: Lens.Lens' TraceSummary (Prelude.Maybe [ResourceARNDetail])
traceSummary_resourceARNs = Lens.lens (\TraceSummary' {resourceARNs} -> resourceARNs) (\s@TraceSummary' {} a -> s {resourceARNs = a} :: TraceSummary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TraceSummary where
  parseJSON =
    Core.withObject
      "TraceSummary"
      ( \x ->
          TraceSummary'
            Prelude.<$> (x Core..:? "Http")
            Prelude.<*> ( x Core..:? "ResponseTimeRootCauses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "HasFault")
            Prelude.<*> (x Core..:? "IsPartial")
            Prelude.<*> ( x Core..:? "AvailabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "MatchedEventTime")
            Prelude.<*> (x Core..:? "Revision")
            Prelude.<*> ( x Core..:? "ErrorRootCauses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Users" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "HasError")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Annotations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Duration")
            Prelude.<*> (x Core..:? "HasThrottle")
            Prelude.<*> ( x Core..:? "FaultRootCauses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ServiceIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ResponseTime")
            Prelude.<*> (x Core..:? "EntryPoint")
            Prelude.<*> (x Core..:? "InstanceIds" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ResourceARNs" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TraceSummary where
  hashWithSalt _salt TraceSummary' {..} =
    _salt `Prelude.hashWithSalt` http
      `Prelude.hashWithSalt` responseTimeRootCauses
      `Prelude.hashWithSalt` hasFault
      `Prelude.hashWithSalt` isPartial
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` matchedEventTime
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` errorRootCauses
      `Prelude.hashWithSalt` users
      `Prelude.hashWithSalt` hasError
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` annotations
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` hasThrottle
      `Prelude.hashWithSalt` faultRootCauses
      `Prelude.hashWithSalt` serviceIds
      `Prelude.hashWithSalt` responseTime
      `Prelude.hashWithSalt` entryPoint
      `Prelude.hashWithSalt` instanceIds
      `Prelude.hashWithSalt` resourceARNs

instance Prelude.NFData TraceSummary where
  rnf TraceSummary' {..} =
    Prelude.rnf http
      `Prelude.seq` Prelude.rnf responseTimeRootCauses
      `Prelude.seq` Prelude.rnf hasFault
      `Prelude.seq` Prelude.rnf isPartial
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf matchedEventTime
      `Prelude.seq` Prelude.rnf revision
      `Prelude.seq` Prelude.rnf errorRootCauses
      `Prelude.seq` Prelude.rnf users
      `Prelude.seq` Prelude.rnf hasError
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf annotations
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf hasThrottle
      `Prelude.seq` Prelude.rnf faultRootCauses
      `Prelude.seq` Prelude.rnf serviceIds
      `Prelude.seq` Prelude.rnf responseTime
      `Prelude.seq` Prelude.rnf entryPoint
      `Prelude.seq` Prelude.rnf instanceIds
      `Prelude.seq` Prelude.rnf resourceARNs
