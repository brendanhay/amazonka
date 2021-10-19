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
    describeOfferingResponse_resourceSpecification,
    describeOfferingResponse_currencyCode,
    describeOfferingResponse_arn,
    describeOfferingResponse_offeringId,
    describeOfferingResponse_region,
    describeOfferingResponse_offeringType,
    describeOfferingResponse_usagePrice,
    describeOfferingResponse_fixedPrice,
    describeOfferingResponse_durationUnits,
    describeOfferingResponse_offeringDescription,
    describeOfferingResponse_duration,
    describeOfferingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeOfferingRequest
--
-- /See:/ 'newDescribeOffering' smart constructor.
data DescribeOffering = DescribeOffering'
  { -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeOffering
newDescribeOffering pOfferingId_ =
  DescribeOffering' {offeringId = pOfferingId_}

-- | Unique offering ID, e.g. \'87654321\'
describeOffering_offeringId :: Lens.Lens' DescribeOffering Prelude.Text
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
            Prelude.<$> (x Core..?> "resourceSpecification")
            Prelude.<*> (x Core..?> "currencyCode")
            Prelude.<*> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "offeringId")
            Prelude.<*> (x Core..?> "region")
            Prelude.<*> (x Core..?> "offeringType")
            Prelude.<*> (x Core..?> "usagePrice")
            Prelude.<*> (x Core..?> "fixedPrice")
            Prelude.<*> (x Core..?> "durationUnits")
            Prelude.<*> (x Core..?> "offeringDescription")
            Prelude.<*> (x Core..?> "duration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOffering

instance Prelude.NFData DescribeOffering

instance Core.ToHeaders DescribeOffering where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeOffering where
  toPath DescribeOffering' {..} =
    Prelude.mconcat
      ["/prod/offerings/", Core.toBS offeringId]

instance Core.ToQuery DescribeOffering where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeOfferingResponse
--
-- /See:/ 'newDescribeOfferingResponse' smart constructor.
data DescribeOfferingResponse = DescribeOfferingResponse'
  { -- | Resource configuration details
    resourceSpecification :: Prelude.Maybe ReservationResourceSpecification,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | Unique offering ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
    arn :: Prelude.Maybe Prelude.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Prelude.Maybe Prelude.Text,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Prelude.Maybe OfferingType,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Prelude.Maybe OfferingDurationUnits,
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Prelude.Maybe Prelude.Text,
    -- | Lease duration, e.g. \'12\'
    duration :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOfferingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSpecification', 'describeOfferingResponse_resourceSpecification' - Resource configuration details
--
-- 'currencyCode', 'describeOfferingResponse_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'arn', 'describeOfferingResponse_arn' - Unique offering ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
--
-- 'offeringId', 'describeOfferingResponse_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'region', 'describeOfferingResponse_region' - AWS region, e.g. \'us-west-2\'
--
-- 'offeringType', 'describeOfferingResponse_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'usagePrice', 'describeOfferingResponse_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'fixedPrice', 'describeOfferingResponse_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'durationUnits', 'describeOfferingResponse_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'offeringDescription', 'describeOfferingResponse_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'duration', 'describeOfferingResponse_duration' - Lease duration, e.g. \'12\'
--
-- 'httpStatus', 'describeOfferingResponse_httpStatus' - The response's http status code.
newDescribeOfferingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOfferingResponse
newDescribeOfferingResponse pHttpStatus_ =
  DescribeOfferingResponse'
    { resourceSpecification =
        Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      arn = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      region = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      durationUnits = Prelude.Nothing,
      offeringDescription = Prelude.Nothing,
      duration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Resource configuration details
describeOfferingResponse_resourceSpecification :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe ReservationResourceSpecification)
describeOfferingResponse_resourceSpecification = Lens.lens (\DescribeOfferingResponse' {resourceSpecification} -> resourceSpecification) (\s@DescribeOfferingResponse' {} a -> s {resourceSpecification = a} :: DescribeOfferingResponse)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
describeOfferingResponse_currencyCode :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Text)
describeOfferingResponse_currencyCode = Lens.lens (\DescribeOfferingResponse' {currencyCode} -> currencyCode) (\s@DescribeOfferingResponse' {} a -> s {currencyCode = a} :: DescribeOfferingResponse)

-- | Unique offering ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
describeOfferingResponse_arn :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Text)
describeOfferingResponse_arn = Lens.lens (\DescribeOfferingResponse' {arn} -> arn) (\s@DescribeOfferingResponse' {} a -> s {arn = a} :: DescribeOfferingResponse)

-- | Unique offering ID, e.g. \'87654321\'
describeOfferingResponse_offeringId :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Text)
describeOfferingResponse_offeringId = Lens.lens (\DescribeOfferingResponse' {offeringId} -> offeringId) (\s@DescribeOfferingResponse' {} a -> s {offeringId = a} :: DescribeOfferingResponse)

-- | AWS region, e.g. \'us-west-2\'
describeOfferingResponse_region :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Text)
describeOfferingResponse_region = Lens.lens (\DescribeOfferingResponse' {region} -> region) (\s@DescribeOfferingResponse' {} a -> s {region = a} :: DescribeOfferingResponse)

-- | Offering type, e.g. \'NO_UPFRONT\'
describeOfferingResponse_offeringType :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe OfferingType)
describeOfferingResponse_offeringType = Lens.lens (\DescribeOfferingResponse' {offeringType} -> offeringType) (\s@DescribeOfferingResponse' {} a -> s {offeringType = a} :: DescribeOfferingResponse)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
describeOfferingResponse_usagePrice :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Double)
describeOfferingResponse_usagePrice = Lens.lens (\DescribeOfferingResponse' {usagePrice} -> usagePrice) (\s@DescribeOfferingResponse' {} a -> s {usagePrice = a} :: DescribeOfferingResponse)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
describeOfferingResponse_fixedPrice :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Double)
describeOfferingResponse_fixedPrice = Lens.lens (\DescribeOfferingResponse' {fixedPrice} -> fixedPrice) (\s@DescribeOfferingResponse' {} a -> s {fixedPrice = a} :: DescribeOfferingResponse)

-- | Units for duration, e.g. \'MONTHS\'
describeOfferingResponse_durationUnits :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe OfferingDurationUnits)
describeOfferingResponse_durationUnits = Lens.lens (\DescribeOfferingResponse' {durationUnits} -> durationUnits) (\s@DescribeOfferingResponse' {} a -> s {durationUnits = a} :: DescribeOfferingResponse)

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
describeOfferingResponse_offeringDescription :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Text)
describeOfferingResponse_offeringDescription = Lens.lens (\DescribeOfferingResponse' {offeringDescription} -> offeringDescription) (\s@DescribeOfferingResponse' {} a -> s {offeringDescription = a} :: DescribeOfferingResponse)

-- | Lease duration, e.g. \'12\'
describeOfferingResponse_duration :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Int)
describeOfferingResponse_duration = Lens.lens (\DescribeOfferingResponse' {duration} -> duration) (\s@DescribeOfferingResponse' {} a -> s {duration = a} :: DescribeOfferingResponse)

-- | The response's http status code.
describeOfferingResponse_httpStatus :: Lens.Lens' DescribeOfferingResponse Prelude.Int
describeOfferingResponse_httpStatus = Lens.lens (\DescribeOfferingResponse' {httpStatus} -> httpStatus) (\s@DescribeOfferingResponse' {} a -> s {httpStatus = a} :: DescribeOfferingResponse)

instance Prelude.NFData DescribeOfferingResponse
