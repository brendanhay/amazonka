{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ArcZonalShift.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ArcZonalShift.Lens
  ( -- * Operations

    -- ** CancelZonalShift
    cancelZonalShift_zonalShiftId,
    zonalShift_awayFrom,
    zonalShift_comment,
    zonalShift_expiryTime,
    zonalShift_resourceIdentifier,
    zonalShift_startTime,
    zonalShift_status,
    zonalShift_zonalShiftId,

    -- ** GetManagedResource
    getManagedResource_resourceIdentifier,
    getManagedResourceResponse_arn,
    getManagedResourceResponse_name,
    getManagedResourceResponse_httpStatus,
    getManagedResourceResponse_appliedWeights,
    getManagedResourceResponse_zonalShifts,

    -- ** ListManagedResources
    listManagedResources_maxResults,
    listManagedResources_nextToken,
    listManagedResourcesResponse_nextToken,
    listManagedResourcesResponse_httpStatus,
    listManagedResourcesResponse_items,

    -- ** ListZonalShifts
    listZonalShifts_maxResults,
    listZonalShifts_nextToken,
    listZonalShifts_status,
    listZonalShiftsResponse_items,
    listZonalShiftsResponse_nextToken,
    listZonalShiftsResponse_httpStatus,

    -- ** StartZonalShift
    startZonalShift_awayFrom,
    startZonalShift_comment,
    startZonalShift_expiresIn,
    startZonalShift_resourceIdentifier,
    zonalShift_awayFrom,
    zonalShift_comment,
    zonalShift_expiryTime,
    zonalShift_resourceIdentifier,
    zonalShift_startTime,
    zonalShift_status,
    zonalShift_zonalShiftId,

    -- ** UpdateZonalShift
    updateZonalShift_comment,
    updateZonalShift_expiresIn,
    updateZonalShift_zonalShiftId,
    zonalShift_awayFrom,
    zonalShift_comment,
    zonalShift_expiryTime,
    zonalShift_resourceIdentifier,
    zonalShift_startTime,
    zonalShift_status,
    zonalShift_zonalShiftId,

    -- * Types

    -- ** ManagedResourceSummary
    managedResourceSummary_arn,
    managedResourceSummary_name,
    managedResourceSummary_availabilityZones,

    -- ** ZonalShift
    zonalShift_awayFrom,
    zonalShift_comment,
    zonalShift_expiryTime,
    zonalShift_resourceIdentifier,
    zonalShift_startTime,
    zonalShift_status,
    zonalShift_zonalShiftId,

    -- ** ZonalShiftInResource
    zonalShiftInResource_appliedStatus,
    zonalShiftInResource_awayFrom,
    zonalShiftInResource_comment,
    zonalShiftInResource_expiryTime,
    zonalShiftInResource_resourceIdentifier,
    zonalShiftInResource_startTime,
    zonalShiftInResource_zonalShiftId,

    -- ** ZonalShiftSummary
    zonalShiftSummary_awayFrom,
    zonalShiftSummary_comment,
    zonalShiftSummary_expiryTime,
    zonalShiftSummary_resourceIdentifier,
    zonalShiftSummary_startTime,
    zonalShiftSummary_status,
    zonalShiftSummary_zonalShiftId,
  )
where

import Amazonka.ArcZonalShift.CancelZonalShift
import Amazonka.ArcZonalShift.GetManagedResource
import Amazonka.ArcZonalShift.ListManagedResources
import Amazonka.ArcZonalShift.ListZonalShifts
import Amazonka.ArcZonalShift.StartZonalShift
import Amazonka.ArcZonalShift.Types.ManagedResourceSummary
import Amazonka.ArcZonalShift.Types.ZonalShift
import Amazonka.ArcZonalShift.Types.ZonalShiftInResource
import Amazonka.ArcZonalShift.Types.ZonalShiftSummary
import Amazonka.ArcZonalShift.UpdateZonalShift
