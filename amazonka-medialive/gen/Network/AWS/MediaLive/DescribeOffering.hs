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
-- Module      : Network.AWS.MediaLive.DescribeOffering
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details for an offering.
module Network.AWS.MediaLive.DescribeOffering
  ( -- * Creating a Request
    DescribeOffering (..),
    newDescribeOffering,

    -- * Request Lenses
    describeOffering_offeringId,

    -- * Destructuring the Response
    DescribeOfferingResponse (..),
    newDescribeOfferingResponse,

    -- * Response Lenses
    describeOfferingResponse_duration,
    describeOfferingResponse_durationUnits,
    describeOfferingResponse_arn,
    describeOfferingResponse_offeringId,
    describeOfferingResponse_currencyCode,
    describeOfferingResponse_resourceSpecification,
    describeOfferingResponse_offeringDescription,
    describeOfferingResponse_fixedPrice,
    describeOfferingResponse_usagePrice,
    describeOfferingResponse_offeringType,
    describeOfferingResponse_region,
    describeOfferingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeOfferingRequest
--
-- /See:/ 'newDescribeOffering' smart constructor.
data DescribeOffering = DescribeOffering'
  { -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOffering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'offeringId', 'describeOffering_offeringId' - Unique offering ID, e.g. \'87654321\'
newDescribeOffering ::
  -- | 'offeringId'
  Core.Text ->
  DescribeOffering
newDescribeOffering pOfferingId_ =
  DescribeOffering' {offeringId = pOfferingId_}

-- | Unique offering ID, e.g. \'87654321\'
describeOffering_offeringId :: Lens.Lens' DescribeOffering Core.Text
describeOffering_offeringId = Lens.lens (\DescribeOffering' {offeringId} -> offeringId) (\s@DescribeOffering' {} a -> s {offeringId = a} :: DescribeOffering)

instance Core.AWSRequest DescribeOffering where
  type
    AWSResponse DescribeOffering =
      DescribeOfferingResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOfferingResponse'
            Core.<$> (x Core..?> "duration")
            Core.<*> (x Core..?> "durationUnits")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "offeringId")
            Core.<*> (x Core..?> "currencyCode")
            Core.<*> (x Core..?> "resourceSpecification")
            Core.<*> (x Core..?> "offeringDescription")
            Core.<*> (x Core..?> "fixedPrice")
            Core.<*> (x Core..?> "usagePrice")
            Core.<*> (x Core..?> "offeringType")
            Core.<*> (x Core..?> "region")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeOffering

instance Core.NFData DescribeOffering

instance Core.ToHeaders DescribeOffering where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeOffering where
  toPath DescribeOffering' {..} =
    Core.mconcat
      ["/prod/offerings/", Core.toBS offeringId]

instance Core.ToQuery DescribeOffering where
  toQuery = Core.const Core.mempty

-- | Placeholder documentation for DescribeOfferingResponse
--
-- /See:/ 'newDescribeOfferingResponse' smart constructor.
data DescribeOfferingResponse = DescribeOfferingResponse'
  { -- | Lease duration, e.g. \'12\'
    duration :: Core.Maybe Core.Int,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Core.Maybe OfferingDurationUnits,
    -- | Unique offering ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
    arn :: Core.Maybe Core.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Core.Maybe Core.Text,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Core.Maybe Core.Text,
    -- | Resource configuration details
    resourceSpecification :: Core.Maybe ReservationResourceSpecification,
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Core.Maybe Core.Text,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Core.Maybe Core.Double,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Core.Maybe Core.Double,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Core.Maybe OfferingType,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'duration', 'describeOfferingResponse_duration' - Lease duration, e.g. \'12\'
--
-- 'durationUnits', 'describeOfferingResponse_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'arn', 'describeOfferingResponse_arn' - Unique offering ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
--
-- 'offeringId', 'describeOfferingResponse_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'currencyCode', 'describeOfferingResponse_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'resourceSpecification', 'describeOfferingResponse_resourceSpecification' - Resource configuration details
--
-- 'offeringDescription', 'describeOfferingResponse_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'fixedPrice', 'describeOfferingResponse_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'usagePrice', 'describeOfferingResponse_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'offeringType', 'describeOfferingResponse_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'region', 'describeOfferingResponse_region' - AWS region, e.g. \'us-west-2\'
--
-- 'httpStatus', 'describeOfferingResponse_httpStatus' - The response's http status code.
newDescribeOfferingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeOfferingResponse
newDescribeOfferingResponse pHttpStatus_ =
  DescribeOfferingResponse'
    { duration = Core.Nothing,
      durationUnits = Core.Nothing,
      arn = Core.Nothing,
      offeringId = Core.Nothing,
      currencyCode = Core.Nothing,
      resourceSpecification = Core.Nothing,
      offeringDescription = Core.Nothing,
      fixedPrice = Core.Nothing,
      usagePrice = Core.Nothing,
      offeringType = Core.Nothing,
      region = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Lease duration, e.g. \'12\'
describeOfferingResponse_duration :: Lens.Lens' DescribeOfferingResponse (Core.Maybe Core.Int)
describeOfferingResponse_duration = Lens.lens (\DescribeOfferingResponse' {duration} -> duration) (\s@DescribeOfferingResponse' {} a -> s {duration = a} :: DescribeOfferingResponse)

-- | Units for duration, e.g. \'MONTHS\'
describeOfferingResponse_durationUnits :: Lens.Lens' DescribeOfferingResponse (Core.Maybe OfferingDurationUnits)
describeOfferingResponse_durationUnits = Lens.lens (\DescribeOfferingResponse' {durationUnits} -> durationUnits) (\s@DescribeOfferingResponse' {} a -> s {durationUnits = a} :: DescribeOfferingResponse)

-- | Unique offering ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
describeOfferingResponse_arn :: Lens.Lens' DescribeOfferingResponse (Core.Maybe Core.Text)
describeOfferingResponse_arn = Lens.lens (\DescribeOfferingResponse' {arn} -> arn) (\s@DescribeOfferingResponse' {} a -> s {arn = a} :: DescribeOfferingResponse)

-- | Unique offering ID, e.g. \'87654321\'
describeOfferingResponse_offeringId :: Lens.Lens' DescribeOfferingResponse (Core.Maybe Core.Text)
describeOfferingResponse_offeringId = Lens.lens (\DescribeOfferingResponse' {offeringId} -> offeringId) (\s@DescribeOfferingResponse' {} a -> s {offeringId = a} :: DescribeOfferingResponse)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
describeOfferingResponse_currencyCode :: Lens.Lens' DescribeOfferingResponse (Core.Maybe Core.Text)
describeOfferingResponse_currencyCode = Lens.lens (\DescribeOfferingResponse' {currencyCode} -> currencyCode) (\s@DescribeOfferingResponse' {} a -> s {currencyCode = a} :: DescribeOfferingResponse)

-- | Resource configuration details
describeOfferingResponse_resourceSpecification :: Lens.Lens' DescribeOfferingResponse (Core.Maybe ReservationResourceSpecification)
describeOfferingResponse_resourceSpecification = Lens.lens (\DescribeOfferingResponse' {resourceSpecification} -> resourceSpecification) (\s@DescribeOfferingResponse' {} a -> s {resourceSpecification = a} :: DescribeOfferingResponse)

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
describeOfferingResponse_offeringDescription :: Lens.Lens' DescribeOfferingResponse (Core.Maybe Core.Text)
describeOfferingResponse_offeringDescription = Lens.lens (\DescribeOfferingResponse' {offeringDescription} -> offeringDescription) (\s@DescribeOfferingResponse' {} a -> s {offeringDescription = a} :: DescribeOfferingResponse)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
describeOfferingResponse_fixedPrice :: Lens.Lens' DescribeOfferingResponse (Core.Maybe Core.Double)
describeOfferingResponse_fixedPrice = Lens.lens (\DescribeOfferingResponse' {fixedPrice} -> fixedPrice) (\s@DescribeOfferingResponse' {} a -> s {fixedPrice = a} :: DescribeOfferingResponse)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
describeOfferingResponse_usagePrice :: Lens.Lens' DescribeOfferingResponse (Core.Maybe Core.Double)
describeOfferingResponse_usagePrice = Lens.lens (\DescribeOfferingResponse' {usagePrice} -> usagePrice) (\s@DescribeOfferingResponse' {} a -> s {usagePrice = a} :: DescribeOfferingResponse)

-- | Offering type, e.g. \'NO_UPFRONT\'
describeOfferingResponse_offeringType :: Lens.Lens' DescribeOfferingResponse (Core.Maybe OfferingType)
describeOfferingResponse_offeringType = Lens.lens (\DescribeOfferingResponse' {offeringType} -> offeringType) (\s@DescribeOfferingResponse' {} a -> s {offeringType = a} :: DescribeOfferingResponse)

-- | AWS region, e.g. \'us-west-2\'
describeOfferingResponse_region :: Lens.Lens' DescribeOfferingResponse (Core.Maybe Core.Text)
describeOfferingResponse_region = Lens.lens (\DescribeOfferingResponse' {region} -> region) (\s@DescribeOfferingResponse' {} a -> s {region = a} :: DescribeOfferingResponse)

-- | The response's http status code.
describeOfferingResponse_httpStatus :: Lens.Lens' DescribeOfferingResponse Core.Int
describeOfferingResponse_httpStatus = Lens.lens (\DescribeOfferingResponse' {httpStatus} -> httpStatus) (\s@DescribeOfferingResponse' {} a -> s {httpStatus = a} :: DescribeOfferingResponse)

instance Core.NFData DescribeOfferingResponse
