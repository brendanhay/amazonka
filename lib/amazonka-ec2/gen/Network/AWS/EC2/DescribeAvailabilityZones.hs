{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeAvailabilityZones
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Availability Zones, Local Zones, and Wavelength Zones that are available to you. If there is an event impacting a zone, you can use this request to view the state and any provided messages for that zone.
--
-- For more information about Availability Zones, Local Zones, and Wavelength Zones, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-regions-availability-zones.html Regions, Zones and Outposts> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeAvailabilityZones
  ( -- * Creating a request
    DescribeAvailabilityZones (..),
    mkDescribeAvailabilityZones,

    -- ** Request lenses
    dazZoneNames,
    dazAllAvailabilityZones,
    dazZoneIds,
    dazFilters,
    dazDryRun,

    -- * Destructuring the response
    DescribeAvailabilityZonesResponse (..),
    mkDescribeAvailabilityZonesResponse,

    -- ** Response lenses
    dazrsAvailabilityZones,
    dazrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAvailabilityZones' smart constructor.
data DescribeAvailabilityZones = DescribeAvailabilityZones'
  { zoneNames ::
      Lude.Maybe [Lude.Text],
    allAvailabilityZones ::
      Lude.Maybe Lude.Bool,
    zoneIds :: Lude.Maybe [Lude.Text],
    filters :: Lude.Maybe [Filter],
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAvailabilityZones' with the minimum fields required to make a request.
--
-- * 'allAvailabilityZones' - Include all Availability Zones, Local Zones, and Wavelength Zones regardless of your opt-in status.
--
-- If you do not use this parameter, the results include only the zones for the Regions where you have chosen the option to opt in.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'filters' - The filters.
--
--
--     * @group-name@ - For Availability Zones, use the Region name. For Local Zones, use the name of the group associated with the Local Zone (for example, @us-west-2-lax-1@ ) For Wavelength Zones, use the name of the group associated with the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
--
--
--     * @message@ - The Zone message.
--
--
--     * @opt-in-status@ - The opt-in status (@opted-in@ , and @not-opted-in@ | @opt-in-not-required@ ).
--
--
--     * @parent-zoneID@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.
--
--
--     * @parent-zoneName@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.
--
--
--     * @region-name@ - The name of the Region for the Zone (for example, @us-east-1@ ).
--
--
--     * @state@ - The state of the Availability Zone, the Local Zone, or the Wavelength Zone (@available@ | @information@ | @impaired@ | @unavailable@ ).
--
--
--     * @zone-id@ - The ID of the Availability Zone (for example, @use1-az1@ ), the Local Zone (for example, @usw2-lax1-az1@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
--
--
--     * @zone-type@ - The type of zone, for example, @local-zone@ .
--
--
--     * @zone-name@ - The name of the Availability Zone (for example, @us-east-1a@ ), the Local Zone (for example, @us-west-2-lax-1a@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
--
--
--     * @zone-type@ - The type of zone, for example, @local-zone@ .
--
--
-- * 'zoneIds' - The IDs of the Availability Zones, Local Zones, and Wavelength Zones.
-- * 'zoneNames' - The names of the Availability Zones, Local Zones, and Wavelength Zones.
mkDescribeAvailabilityZones ::
  DescribeAvailabilityZones
mkDescribeAvailabilityZones =
  DescribeAvailabilityZones'
    { zoneNames = Lude.Nothing,
      allAvailabilityZones = Lude.Nothing,
      zoneIds = Lude.Nothing,
      filters = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | The names of the Availability Zones, Local Zones, and Wavelength Zones.
--
-- /Note:/ Consider using 'zoneNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazZoneNames :: Lens.Lens' DescribeAvailabilityZones (Lude.Maybe [Lude.Text])
dazZoneNames = Lens.lens (zoneNames :: DescribeAvailabilityZones -> Lude.Maybe [Lude.Text]) (\s a -> s {zoneNames = a} :: DescribeAvailabilityZones)
{-# DEPRECATED dazZoneNames "Use generic-lens or generic-optics with 'zoneNames' instead." #-}

-- | Include all Availability Zones, Local Zones, and Wavelength Zones regardless of your opt-in status.
--
-- If you do not use this parameter, the results include only the zones for the Regions where you have chosen the option to opt in.
--
-- /Note:/ Consider using 'allAvailabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazAllAvailabilityZones :: Lens.Lens' DescribeAvailabilityZones (Lude.Maybe Lude.Bool)
dazAllAvailabilityZones = Lens.lens (allAvailabilityZones :: DescribeAvailabilityZones -> Lude.Maybe Lude.Bool) (\s a -> s {allAvailabilityZones = a} :: DescribeAvailabilityZones)
{-# DEPRECATED dazAllAvailabilityZones "Use generic-lens or generic-optics with 'allAvailabilityZones' instead." #-}

-- | The IDs of the Availability Zones, Local Zones, and Wavelength Zones.
--
-- /Note:/ Consider using 'zoneIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazZoneIds :: Lens.Lens' DescribeAvailabilityZones (Lude.Maybe [Lude.Text])
dazZoneIds = Lens.lens (zoneIds :: DescribeAvailabilityZones -> Lude.Maybe [Lude.Text]) (\s a -> s {zoneIds = a} :: DescribeAvailabilityZones)
{-# DEPRECATED dazZoneIds "Use generic-lens or generic-optics with 'zoneIds' instead." #-}

-- | The filters.
--
--
--     * @group-name@ - For Availability Zones, use the Region name. For Local Zones, use the name of the group associated with the Local Zone (for example, @us-west-2-lax-1@ ) For Wavelength Zones, use the name of the group associated with the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
--
--
--     * @message@ - The Zone message.
--
--
--     * @opt-in-status@ - The opt-in status (@opted-in@ , and @not-opted-in@ | @opt-in-not-required@ ).
--
--
--     * @parent-zoneID@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.
--
--
--     * @parent-zoneName@ - The ID of the zone that handles some of the Local Zone and Wavelength Zone control plane operations, such as API calls.
--
--
--     * @region-name@ - The name of the Region for the Zone (for example, @us-east-1@ ).
--
--
--     * @state@ - The state of the Availability Zone, the Local Zone, or the Wavelength Zone (@available@ | @information@ | @impaired@ | @unavailable@ ).
--
--
--     * @zone-id@ - The ID of the Availability Zone (for example, @use1-az1@ ), the Local Zone (for example, @usw2-lax1-az1@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
--
--
--     * @zone-type@ - The type of zone, for example, @local-zone@ .
--
--
--     * @zone-name@ - The name of the Availability Zone (for example, @us-east-1a@ ), the Local Zone (for example, @us-west-2-lax-1a@ ), or the Wavelength Zone (for example, @us-east-1-wl1-bos-wlz-1@ ).
--
--
--     * @zone-type@ - The type of zone, for example, @local-zone@ .
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazFilters :: Lens.Lens' DescribeAvailabilityZones (Lude.Maybe [Filter])
dazFilters = Lens.lens (filters :: DescribeAvailabilityZones -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeAvailabilityZones)
{-# DEPRECATED dazFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazDryRun :: Lens.Lens' DescribeAvailabilityZones (Lude.Maybe Lude.Bool)
dazDryRun = Lens.lens (dryRun :: DescribeAvailabilityZones -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeAvailabilityZones)
{-# DEPRECATED dazDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeAvailabilityZones where
  type
    Rs DescribeAvailabilityZones =
      DescribeAvailabilityZonesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeAvailabilityZonesResponse'
            Lude.<$> ( x Lude..@? "availabilityZoneInfo" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAvailabilityZones where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAvailabilityZones where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAvailabilityZones where
  toQuery DescribeAvailabilityZones' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeAvailabilityZones" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "ZoneName" Lude.<$> zoneNames),
        "AllAvailabilityZones" Lude.=: allAvailabilityZones,
        Lude.toQuery (Lude.toQueryList "ZoneId" Lude.<$> zoneIds),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeAvailabilityZonesResponse' smart constructor.
data DescribeAvailabilityZonesResponse = DescribeAvailabilityZonesResponse'
  { availabilityZones ::
      Lude.Maybe
        [AvailabilityZone],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAvailabilityZonesResponse' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - Information about the Availability Zones, Local Zones, and Wavelength Zones.
-- * 'responseStatus' - The response status code.
mkDescribeAvailabilityZonesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAvailabilityZonesResponse
mkDescribeAvailabilityZonesResponse pResponseStatus_ =
  DescribeAvailabilityZonesResponse'
    { availabilityZones =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the Availability Zones, Local Zones, and Wavelength Zones.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazrsAvailabilityZones :: Lens.Lens' DescribeAvailabilityZonesResponse (Lude.Maybe [AvailabilityZone])
dazrsAvailabilityZones = Lens.lens (availabilityZones :: DescribeAvailabilityZonesResponse -> Lude.Maybe [AvailabilityZone]) (\s a -> s {availabilityZones = a} :: DescribeAvailabilityZonesResponse)
{-# DEPRECATED dazrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazrsResponseStatus :: Lens.Lens' DescribeAvailabilityZonesResponse Lude.Int
dazrsResponseStatus = Lens.lens (responseStatus :: DescribeAvailabilityZonesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAvailabilityZonesResponse)
{-# DEPRECATED dazrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
