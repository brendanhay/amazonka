{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeOffering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details for an offering.
module Network.AWS.MediaLive.DescribeOffering
  ( -- * Creating a request
    DescribeOffering (..),
    mkDescribeOffering,

    -- ** Request lenses
    doOfferingId,

    -- * Destructuring the response
    DescribeOfferingResponse (..),
    mkDescribeOfferingResponse,

    -- ** Response lenses
    dorsResourceSpecification,
    dorsCurrencyCode,
    dorsARN,
    dorsOfferingId,
    dorsRegion,
    dorsOfferingType,
    dorsUsagePrice,
    dorsFixedPrice,
    dorsDurationUnits,
    dorsOfferingDescription,
    dorsDuration,
    dorsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for DescribeOfferingRequest
--
-- /See:/ 'mkDescribeOffering' smart constructor.
newtype DescribeOffering = DescribeOffering'
  { -- | Unique offering ID, e.g. '87654321'
    offeringId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOffering' with the minimum fields required to make a request.
--
-- * 'offeringId' - Unique offering ID, e.g. '87654321'
mkDescribeOffering ::
  -- | 'offeringId'
  Lude.Text ->
  DescribeOffering
mkDescribeOffering pOfferingId_ =
  DescribeOffering' {offeringId = pOfferingId_}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doOfferingId :: Lens.Lens' DescribeOffering Lude.Text
doOfferingId = Lens.lens (offeringId :: DescribeOffering -> Lude.Text) (\s a -> s {offeringId = a} :: DescribeOffering)
{-# DEPRECATED doOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

instance Lude.AWSRequest DescribeOffering where
  type Rs DescribeOffering = DescribeOfferingResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOfferingResponse'
            Lude.<$> (x Lude..?> "resourceSpecification")
            Lude.<*> (x Lude..?> "currencyCode")
            Lude.<*> (x Lude..?> "arn")
            Lude.<*> (x Lude..?> "offeringId")
            Lude.<*> (x Lude..?> "region")
            Lude.<*> (x Lude..?> "offeringType")
            Lude.<*> (x Lude..?> "usagePrice")
            Lude.<*> (x Lude..?> "fixedPrice")
            Lude.<*> (x Lude..?> "durationUnits")
            Lude.<*> (x Lude..?> "offeringDescription")
            Lude.<*> (x Lude..?> "duration")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeOffering where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DescribeOffering where
  toPath DescribeOffering' {..} =
    Lude.mconcat ["/prod/offerings/", Lude.toBS offeringId]

instance Lude.ToQuery DescribeOffering where
  toQuery = Lude.const Lude.mempty

-- | Placeholder documentation for DescribeOfferingResponse
--
-- /See:/ 'mkDescribeOfferingResponse' smart constructor.
data DescribeOfferingResponse = DescribeOfferingResponse'
  { -- | Resource configuration details
    resourceSpecification :: Lude.Maybe ReservationResourceSpecification,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
    currencyCode :: Lude.Maybe Lude.Text,
    -- | Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
    arn :: Lude.Maybe Lude.Text,
    -- | Unique offering ID, e.g. '87654321'
    offeringId :: Lude.Maybe Lude.Text,
    -- | AWS region, e.g. 'us-west-2'
    region :: Lude.Maybe Lude.Text,
    -- | Offering type, e.g. 'NO_UPFRONT'
    offeringType :: Lude.Maybe OfferingType,
    -- | Recurring usage charge for each reserved resource, e.g. '157.0'
    usagePrice :: Lude.Maybe Lude.Double,
    -- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
    fixedPrice :: Lude.Maybe Lude.Double,
    -- | Units for duration, e.g. 'MONTHS'
    durationUnits :: Lude.Maybe OfferingDurationUnits,
    -- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
    offeringDescription :: Lude.Maybe Lude.Text,
    -- | Lease duration, e.g. '12'
    duration :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeOfferingResponse' with the minimum fields required to make a request.
--
-- * 'resourceSpecification' - Resource configuration details
-- * 'currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
-- * 'arn' - Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
-- * 'offeringId' - Unique offering ID, e.g. '87654321'
-- * 'region' - AWS region, e.g. 'us-west-2'
-- * 'offeringType' - Offering type, e.g. 'NO_UPFRONT'
-- * 'usagePrice' - Recurring usage charge for each reserved resource, e.g. '157.0'
-- * 'fixedPrice' - One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
-- * 'durationUnits' - Units for duration, e.g. 'MONTHS'
-- * 'offeringDescription' - Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
-- * 'duration' - Lease duration, e.g. '12'
-- * 'responseStatus' - The response status code.
mkDescribeOfferingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOfferingResponse
mkDescribeOfferingResponse pResponseStatus_ =
  DescribeOfferingResponse'
    { resourceSpecification = Lude.Nothing,
      currencyCode = Lude.Nothing,
      arn = Lude.Nothing,
      offeringId = Lude.Nothing,
      region = Lude.Nothing,
      offeringType = Lude.Nothing,
      usagePrice = Lude.Nothing,
      fixedPrice = Lude.Nothing,
      durationUnits = Lude.Nothing,
      offeringDescription = Lude.Nothing,
      duration = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Resource configuration details
--
-- /Note:/ Consider using 'resourceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsResourceSpecification :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe ReservationResourceSpecification)
dorsResourceSpecification = Lens.lens (resourceSpecification :: DescribeOfferingResponse -> Lude.Maybe ReservationResourceSpecification) (\s a -> s {resourceSpecification = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsResourceSpecification "Use generic-lens or generic-optics with 'resourceSpecification' instead." #-}

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g. 'USD'
--
-- /Note:/ Consider using 'currencyCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsCurrencyCode :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe Lude.Text)
dorsCurrencyCode = Lens.lens (currencyCode :: DescribeOfferingResponse -> Lude.Maybe Lude.Text) (\s a -> s {currencyCode = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsCurrencyCode "Use generic-lens or generic-optics with 'currencyCode' instead." #-}

-- | Unique offering ARN, e.g. 'arn:aws:medialive:us-west-2:123456789012:offering:87654321'
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsARN :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe Lude.Text)
dorsARN = Lens.lens (arn :: DescribeOfferingResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Unique offering ID, e.g. '87654321'
--
-- /Note:/ Consider using 'offeringId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsOfferingId :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe Lude.Text)
dorsOfferingId = Lens.lens (offeringId :: DescribeOfferingResponse -> Lude.Maybe Lude.Text) (\s a -> s {offeringId = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsOfferingId "Use generic-lens or generic-optics with 'offeringId' instead." #-}

-- | AWS region, e.g. 'us-west-2'
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsRegion :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe Lude.Text)
dorsRegion = Lens.lens (region :: DescribeOfferingResponse -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Offering type, e.g. 'NO_UPFRONT'
--
-- /Note:/ Consider using 'offeringType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsOfferingType :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe OfferingType)
dorsOfferingType = Lens.lens (offeringType :: DescribeOfferingResponse -> Lude.Maybe OfferingType) (\s a -> s {offeringType = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsOfferingType "Use generic-lens or generic-optics with 'offeringType' instead." #-}

-- | Recurring usage charge for each reserved resource, e.g. '157.0'
--
-- /Note:/ Consider using 'usagePrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsUsagePrice :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe Lude.Double)
dorsUsagePrice = Lens.lens (usagePrice :: DescribeOfferingResponse -> Lude.Maybe Lude.Double) (\s a -> s {usagePrice = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsUsagePrice "Use generic-lens or generic-optics with 'usagePrice' instead." #-}

-- | One-time charge for each reserved resource, e.g. '0.0' for a NO_UPFRONT offering
--
-- /Note:/ Consider using 'fixedPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsFixedPrice :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe Lude.Double)
dorsFixedPrice = Lens.lens (fixedPrice :: DescribeOfferingResponse -> Lude.Maybe Lude.Double) (\s a -> s {fixedPrice = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsFixedPrice "Use generic-lens or generic-optics with 'fixedPrice' instead." #-}

-- | Units for duration, e.g. 'MONTHS'
--
-- /Note:/ Consider using 'durationUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsDurationUnits :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe OfferingDurationUnits)
dorsDurationUnits = Lens.lens (durationUnits :: DescribeOfferingResponse -> Lude.Maybe OfferingDurationUnits) (\s a -> s {durationUnits = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsDurationUnits "Use generic-lens or generic-optics with 'durationUnits' instead." #-}

-- | Offering description, e.g. 'HD AVC output at 10-20 Mbps, 30 fps, and standard VQ in US West (Oregon)'
--
-- /Note:/ Consider using 'offeringDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsOfferingDescription :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe Lude.Text)
dorsOfferingDescription = Lens.lens (offeringDescription :: DescribeOfferingResponse -> Lude.Maybe Lude.Text) (\s a -> s {offeringDescription = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsOfferingDescription "Use generic-lens or generic-optics with 'offeringDescription' instead." #-}

-- | Lease duration, e.g. '12'
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsDuration :: Lens.Lens' DescribeOfferingResponse (Lude.Maybe Lude.Int)
dorsDuration = Lens.lens (duration :: DescribeOfferingResponse -> Lude.Maybe Lude.Int) (\s a -> s {duration = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorsResponseStatus :: Lens.Lens' DescribeOfferingResponse Lude.Int
dorsResponseStatus = Lens.lens (responseStatus :: DescribeOfferingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOfferingResponse)
{-# DEPRECATED dorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
