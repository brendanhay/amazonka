{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAvailabilityZones
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Availability Zones, Local Zones, and Wavelength Zones that
-- are available to you. If there is an event impacting a zone, you can use
-- this request to view the state and any provided messages for that zone.
--
-- For more information about Availability Zones, Local Zones, and
-- Wavelength Zones, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html Regions, Zones and Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.DescribeAvailabilityZones
  ( -- * Creating a Request
    DescribeAvailabilityZones (..),
    newDescribeAvailabilityZones,

    -- * Request Lenses
    describeAvailabilityZones_dryRun,
    describeAvailabilityZones_filters,
    describeAvailabilityZones_zoneIds,
    describeAvailabilityZones_allAvailabilityZones,
    describeAvailabilityZones_zoneNames,

    -- * Destructuring the Response
    DescribeAvailabilityZonesResponse (..),
    newDescribeAvailabilityZonesResponse,

    -- * Response Lenses
    describeAvailabilityZonesResponse_availabilityZones,
    describeAvailabilityZonesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAvailabilityZones' smart constructor.
data DescribeAvailabilityZones = DescribeAvailabilityZones'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters.
    --
    -- -   @group-name@ - For Availability Zones, use the Region name. For
    --     Local Zones, use the name of the group associated with the Local
    --     Zone (for example, @us-west-2-lax-1@) For Wavelength Zones, use the
    --     name of the group associated with the Wavelength Zone (for example,
    --     @us-east-1-wl1-bos-wlz-1@).
    --
    -- -   @message@ - The Zone message.
    --
    -- -   @opt-in-status@ - The opt-in status (@opted-in@, and @not-opted-in@
    --     | @opt-in-not-required@).
    --
    -- -   @parent-zoneID@ - The ID of the zone that handles some of the Local
    --     Zone and Wavelength Zone control plane operations, such as API
    --     calls.
    --
    -- -   @parent-zoneName@ - The ID of the zone that handles some of the
    --     Local Zone and Wavelength Zone control plane operations, such as API
    --     calls.
    --
    -- -   @region-name@ - The name of the Region for the Zone (for example,
    --     @us-east-1@).
    --
    -- -   @state@ - The state of the Availability Zone, the Local Zone, or the
    --     Wavelength Zone (@available@ | @information@ | @impaired@ |
    --     @unavailable@).
    --
    -- -   @zone-id@ - The ID of the Availability Zone (for example,
    --     @use1-az1@), the Local Zone (for example, @usw2-lax1-az1@), or the
    --     Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@).
    --
    -- -   @zone-type@ - The type of zone, for example, @local-zone@.
    --
    -- -   @zone-name@ - The name of the Availability Zone (for example,
    --     @us-east-1a@), the Local Zone (for example, @us-west-2-lax-1a@), or
    --     the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@).
    --
    -- -   @zone-type@ - The type of zone, for example, @local-zone@.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the Availability Zones, Local Zones, and Wavelength Zones.
    zoneIds :: Prelude.Maybe [Prelude.Text],
    -- | Include all Availability Zones, Local Zones, and Wavelength Zones
    -- regardless of your opt-in status.
    --
    -- If you do not use this parameter, the results include only the zones for
    -- the Regions where you have chosen the option to opt in.
    allAvailabilityZones :: Prelude.Maybe Prelude.Bool,
    -- | The names of the Availability Zones, Local Zones, and Wavelength Zones.
    zoneNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAvailabilityZones' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeAvailabilityZones_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeAvailabilityZones_filters' - The filters.
--
-- -   @group-name@ - For Availability Zones, use the Region name. For
--     Local Zones, use the name of the group associated with the Local
--     Zone (for example, @us-west-2-lax-1@) For Wavelength Zones, use the
--     name of the group associated with the Wavelength Zone (for example,
--     @us-east-1-wl1-bos-wlz-1@).
--
-- -   @message@ - The Zone message.
--
-- -   @opt-in-status@ - The opt-in status (@opted-in@, and @not-opted-in@
--     | @opt-in-not-required@).
--
-- -   @parent-zoneID@ - The ID of the zone that handles some of the Local
--     Zone and Wavelength Zone control plane operations, such as API
--     calls.
--
-- -   @parent-zoneName@ - The ID of the zone that handles some of the
--     Local Zone and Wavelength Zone control plane operations, such as API
--     calls.
--
-- -   @region-name@ - The name of the Region for the Zone (for example,
--     @us-east-1@).
--
-- -   @state@ - The state of the Availability Zone, the Local Zone, or the
--     Wavelength Zone (@available@ | @information@ | @impaired@ |
--     @unavailable@).
--
-- -   @zone-id@ - The ID of the Availability Zone (for example,
--     @use1-az1@), the Local Zone (for example, @usw2-lax1-az1@), or the
--     Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@).
--
-- -   @zone-type@ - The type of zone, for example, @local-zone@.
--
-- -   @zone-name@ - The name of the Availability Zone (for example,
--     @us-east-1a@), the Local Zone (for example, @us-west-2-lax-1a@), or
--     the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@).
--
-- -   @zone-type@ - The type of zone, for example, @local-zone@.
--
-- 'zoneIds', 'describeAvailabilityZones_zoneIds' - The IDs of the Availability Zones, Local Zones, and Wavelength Zones.
--
-- 'allAvailabilityZones', 'describeAvailabilityZones_allAvailabilityZones' - Include all Availability Zones, Local Zones, and Wavelength Zones
-- regardless of your opt-in status.
--
-- If you do not use this parameter, the results include only the zones for
-- the Regions where you have chosen the option to opt in.
--
-- 'zoneNames', 'describeAvailabilityZones_zoneNames' - The names of the Availability Zones, Local Zones, and Wavelength Zones.
newDescribeAvailabilityZones ::
  DescribeAvailabilityZones
newDescribeAvailabilityZones =
  DescribeAvailabilityZones'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      zoneIds = Prelude.Nothing,
      allAvailabilityZones = Prelude.Nothing,
      zoneNames = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeAvailabilityZones_dryRun :: Lens.Lens' DescribeAvailabilityZones (Prelude.Maybe Prelude.Bool)
describeAvailabilityZones_dryRun = Lens.lens (\DescribeAvailabilityZones' {dryRun} -> dryRun) (\s@DescribeAvailabilityZones' {} a -> s {dryRun = a} :: DescribeAvailabilityZones)

-- | The filters.
--
-- -   @group-name@ - For Availability Zones, use the Region name. For
--     Local Zones, use the name of the group associated with the Local
--     Zone (for example, @us-west-2-lax-1@) For Wavelength Zones, use the
--     name of the group associated with the Wavelength Zone (for example,
--     @us-east-1-wl1-bos-wlz-1@).
--
-- -   @message@ - The Zone message.
--
-- -   @opt-in-status@ - The opt-in status (@opted-in@, and @not-opted-in@
--     | @opt-in-not-required@).
--
-- -   @parent-zoneID@ - The ID of the zone that handles some of the Local
--     Zone and Wavelength Zone control plane operations, such as API
--     calls.
--
-- -   @parent-zoneName@ - The ID of the zone that handles some of the
--     Local Zone and Wavelength Zone control plane operations, such as API
--     calls.
--
-- -   @region-name@ - The name of the Region for the Zone (for example,
--     @us-east-1@).
--
-- -   @state@ - The state of the Availability Zone, the Local Zone, or the
--     Wavelength Zone (@available@ | @information@ | @impaired@ |
--     @unavailable@).
--
-- -   @zone-id@ - The ID of the Availability Zone (for example,
--     @use1-az1@), the Local Zone (for example, @usw2-lax1-az1@), or the
--     Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@).
--
-- -   @zone-type@ - The type of zone, for example, @local-zone@.
--
-- -   @zone-name@ - The name of the Availability Zone (for example,
--     @us-east-1a@), the Local Zone (for example, @us-west-2-lax-1a@), or
--     the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@).
--
-- -   @zone-type@ - The type of zone, for example, @local-zone@.
describeAvailabilityZones_filters :: Lens.Lens' DescribeAvailabilityZones (Prelude.Maybe [Filter])
describeAvailabilityZones_filters = Lens.lens (\DescribeAvailabilityZones' {filters} -> filters) (\s@DescribeAvailabilityZones' {} a -> s {filters = a} :: DescribeAvailabilityZones) Prelude.. Lens.mapping Lens._Coerce

-- | The IDs of the Availability Zones, Local Zones, and Wavelength Zones.
describeAvailabilityZones_zoneIds :: Lens.Lens' DescribeAvailabilityZones (Prelude.Maybe [Prelude.Text])
describeAvailabilityZones_zoneIds = Lens.lens (\DescribeAvailabilityZones' {zoneIds} -> zoneIds) (\s@DescribeAvailabilityZones' {} a -> s {zoneIds = a} :: DescribeAvailabilityZones) Prelude.. Lens.mapping Lens._Coerce

-- | Include all Availability Zones, Local Zones, and Wavelength Zones
-- regardless of your opt-in status.
--
-- If you do not use this parameter, the results include only the zones for
-- the Regions where you have chosen the option to opt in.
describeAvailabilityZones_allAvailabilityZones :: Lens.Lens' DescribeAvailabilityZones (Prelude.Maybe Prelude.Bool)
describeAvailabilityZones_allAvailabilityZones = Lens.lens (\DescribeAvailabilityZones' {allAvailabilityZones} -> allAvailabilityZones) (\s@DescribeAvailabilityZones' {} a -> s {allAvailabilityZones = a} :: DescribeAvailabilityZones)

-- | The names of the Availability Zones, Local Zones, and Wavelength Zones.
describeAvailabilityZones_zoneNames :: Lens.Lens' DescribeAvailabilityZones (Prelude.Maybe [Prelude.Text])
describeAvailabilityZones_zoneNames = Lens.lens (\DescribeAvailabilityZones' {zoneNames} -> zoneNames) (\s@DescribeAvailabilityZones' {} a -> s {zoneNames = a} :: DescribeAvailabilityZones) Prelude.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeAvailabilityZones where
  type
    AWSResponse DescribeAvailabilityZones =
      DescribeAvailabilityZonesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAvailabilityZonesResponse'
            Prelude.<$> ( x Core..@? "availabilityZoneInfo"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAvailabilityZones

instance Prelude.NFData DescribeAvailabilityZones

instance Core.ToHeaders DescribeAvailabilityZones where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeAvailabilityZones where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeAvailabilityZones where
  toQuery DescribeAvailabilityZones' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeAvailabilityZones" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        Core.toQuery
          (Core.toQueryList "ZoneId" Prelude.<$> zoneIds),
        "AllAvailabilityZones" Core.=: allAvailabilityZones,
        Core.toQuery
          (Core.toQueryList "ZoneName" Prelude.<$> zoneNames)
      ]

-- | /See:/ 'newDescribeAvailabilityZonesResponse' smart constructor.
data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse'
  { -- | Information about the Availability Zones, Local Zones, and Wavelength
    -- Zones.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAvailabilityZonesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'describeAvailabilityZonesResponse_availabilityZones' - Information about the Availability Zones, Local Zones, and Wavelength
-- Zones.
--
-- 'httpStatus', 'describeAvailabilityZonesResponse_httpStatus' - The response's http status code.
newDescribeAvailabilityZonesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAvailabilityZonesResponse
newDescribeAvailabilityZonesResponse pHttpStatus_ =
  DescribeAvailabilityZonesResponse'
    { availabilityZones =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Availability Zones, Local Zones, and Wavelength
-- Zones.
describeAvailabilityZonesResponse_availabilityZones :: Lens.Lens' DescribeAvailabilityZonesResponse (Prelude.Maybe [AvailabilityZone])
describeAvailabilityZonesResponse_availabilityZones = Lens.lens (\DescribeAvailabilityZonesResponse' {availabilityZones} -> availabilityZones) (\s@DescribeAvailabilityZonesResponse' {} a -> s {availabilityZones = a} :: DescribeAvailabilityZonesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAvailabilityZonesResponse_httpStatus :: Lens.Lens' DescribeAvailabilityZonesResponse Prelude.Int
describeAvailabilityZonesResponse_httpStatus = Lens.lens (\DescribeAvailabilityZonesResponse' {httpStatus} -> httpStatus) (\s@DescribeAvailabilityZonesResponse' {} a -> s {httpStatus = a} :: DescribeAvailabilityZonesResponse)

instance
  Prelude.NFData
    DescribeAvailabilityZonesResponse
