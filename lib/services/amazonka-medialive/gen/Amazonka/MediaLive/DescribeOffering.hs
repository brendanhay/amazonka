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
-- Module      : Amazonka.MediaLive.DescribeOffering
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details for an offering.
module Amazonka.MediaLive.DescribeOffering
  ( -- * Creating a Request
    DescribeOffering (..),
    newDescribeOffering,

    -- * Request Lenses
    describeOffering_offeringId,

    -- * Destructuring the Response
    DescribeOfferingResponse (..),
    newDescribeOfferingResponse,

    -- * Response Lenses
    describeOfferingResponse_arn,
    describeOfferingResponse_currencyCode,
    describeOfferingResponse_duration,
    describeOfferingResponse_durationUnits,
    describeOfferingResponse_fixedPrice,
    describeOfferingResponse_offeringDescription,
    describeOfferingResponse_offeringId,
    describeOfferingResponse_offeringType,
    describeOfferingResponse_region,
    describeOfferingResponse_resourceSpecification,
    describeOfferingResponse_usagePrice,
    describeOfferingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOfferingResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "currencyCode")
            Prelude.<*> (x Data..?> "duration")
            Prelude.<*> (x Data..?> "durationUnits")
            Prelude.<*> (x Data..?> "fixedPrice")
            Prelude.<*> (x Data..?> "offeringDescription")
            Prelude.<*> (x Data..?> "offeringId")
            Prelude.<*> (x Data..?> "offeringType")
            Prelude.<*> (x Data..?> "region")
            Prelude.<*> (x Data..?> "resourceSpecification")
            Prelude.<*> (x Data..?> "usagePrice")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOffering where
  hashWithSalt _salt DescribeOffering' {..} =
    _salt `Prelude.hashWithSalt` offeringId

instance Prelude.NFData DescribeOffering where
  rnf DescribeOffering' {..} = Prelude.rnf offeringId

instance Data.ToHeaders DescribeOffering where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeOffering where
  toPath DescribeOffering' {..} =
    Prelude.mconcat
      ["/prod/offerings/", Data.toBS offeringId]

instance Data.ToQuery DescribeOffering where
  toQuery = Prelude.const Prelude.mempty

-- | Placeholder documentation for DescribeOfferingResponse
--
-- /See:/ 'newDescribeOfferingResponse' smart constructor.
data DescribeOfferingResponse = DescribeOfferingResponse'
  { -- | Unique offering ARN, e.g.
    -- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
    arn :: Prelude.Maybe Prelude.Text,
    -- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
    -- \'USD\'
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | Lease duration, e.g. \'12\'
    duration :: Prelude.Maybe Prelude.Int,
    -- | Units for duration, e.g. \'MONTHS\'
    durationUnits :: Prelude.Maybe OfferingDurationUnits,
    -- | One-time charge for each reserved resource, e.g. \'0.0\' for a
    -- NO_UPFRONT offering
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
    -- standard VQ in US West (Oregon)\'
    offeringDescription :: Prelude.Maybe Prelude.Text,
    -- | Unique offering ID, e.g. \'87654321\'
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | Offering type, e.g. \'NO_UPFRONT\'
    offeringType :: Prelude.Maybe OfferingType,
    -- | AWS region, e.g. \'us-west-2\'
    region :: Prelude.Maybe Prelude.Text,
    -- | Resource configuration details
    resourceSpecification :: Prelude.Maybe ReservationResourceSpecification,
    -- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
    usagePrice :: Prelude.Maybe Prelude.Double,
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
-- 'arn', 'describeOfferingResponse_arn' - Unique offering ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
--
-- 'currencyCode', 'describeOfferingResponse_currencyCode' - Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
--
-- 'duration', 'describeOfferingResponse_duration' - Lease duration, e.g. \'12\'
--
-- 'durationUnits', 'describeOfferingResponse_durationUnits' - Units for duration, e.g. \'MONTHS\'
--
-- 'fixedPrice', 'describeOfferingResponse_fixedPrice' - One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
--
-- 'offeringDescription', 'describeOfferingResponse_offeringDescription' - Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
--
-- 'offeringId', 'describeOfferingResponse_offeringId' - Unique offering ID, e.g. \'87654321\'
--
-- 'offeringType', 'describeOfferingResponse_offeringType' - Offering type, e.g. \'NO_UPFRONT\'
--
-- 'region', 'describeOfferingResponse_region' - AWS region, e.g. \'us-west-2\'
--
-- 'resourceSpecification', 'describeOfferingResponse_resourceSpecification' - Resource configuration details
--
-- 'usagePrice', 'describeOfferingResponse_usagePrice' - Recurring usage charge for each reserved resource, e.g. \'157.0\'
--
-- 'httpStatus', 'describeOfferingResponse_httpStatus' - The response's http status code.
newDescribeOfferingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOfferingResponse
newDescribeOfferingResponse pHttpStatus_ =
  DescribeOfferingResponse'
    { arn = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      duration = Prelude.Nothing,
      durationUnits = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      offeringDescription = Prelude.Nothing,
      offeringId = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      region = Prelude.Nothing,
      resourceSpecification = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique offering ARN, e.g.
-- \'arn:aws:medialive:us-west-2:123456789012:offering:87654321\'
describeOfferingResponse_arn :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Text)
describeOfferingResponse_arn = Lens.lens (\DescribeOfferingResponse' {arn} -> arn) (\s@DescribeOfferingResponse' {} a -> s {arn = a} :: DescribeOfferingResponse)

-- | Currency code for usagePrice and fixedPrice in ISO-4217 format, e.g.
-- \'USD\'
describeOfferingResponse_currencyCode :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Text)
describeOfferingResponse_currencyCode = Lens.lens (\DescribeOfferingResponse' {currencyCode} -> currencyCode) (\s@DescribeOfferingResponse' {} a -> s {currencyCode = a} :: DescribeOfferingResponse)

-- | Lease duration, e.g. \'12\'
describeOfferingResponse_duration :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Int)
describeOfferingResponse_duration = Lens.lens (\DescribeOfferingResponse' {duration} -> duration) (\s@DescribeOfferingResponse' {} a -> s {duration = a} :: DescribeOfferingResponse)

-- | Units for duration, e.g. \'MONTHS\'
describeOfferingResponse_durationUnits :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe OfferingDurationUnits)
describeOfferingResponse_durationUnits = Lens.lens (\DescribeOfferingResponse' {durationUnits} -> durationUnits) (\s@DescribeOfferingResponse' {} a -> s {durationUnits = a} :: DescribeOfferingResponse)

-- | One-time charge for each reserved resource, e.g. \'0.0\' for a
-- NO_UPFRONT offering
describeOfferingResponse_fixedPrice :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Double)
describeOfferingResponse_fixedPrice = Lens.lens (\DescribeOfferingResponse' {fixedPrice} -> fixedPrice) (\s@DescribeOfferingResponse' {} a -> s {fixedPrice = a} :: DescribeOfferingResponse)

-- | Offering description, e.g. \'HD AVC output at 10-20 Mbps, 30 fps, and
-- standard VQ in US West (Oregon)\'
describeOfferingResponse_offeringDescription :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Text)
describeOfferingResponse_offeringDescription = Lens.lens (\DescribeOfferingResponse' {offeringDescription} -> offeringDescription) (\s@DescribeOfferingResponse' {} a -> s {offeringDescription = a} :: DescribeOfferingResponse)

-- | Unique offering ID, e.g. \'87654321\'
describeOfferingResponse_offeringId :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Text)
describeOfferingResponse_offeringId = Lens.lens (\DescribeOfferingResponse' {offeringId} -> offeringId) (\s@DescribeOfferingResponse' {} a -> s {offeringId = a} :: DescribeOfferingResponse)

-- | Offering type, e.g. \'NO_UPFRONT\'
describeOfferingResponse_offeringType :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe OfferingType)
describeOfferingResponse_offeringType = Lens.lens (\DescribeOfferingResponse' {offeringType} -> offeringType) (\s@DescribeOfferingResponse' {} a -> s {offeringType = a} :: DescribeOfferingResponse)

-- | AWS region, e.g. \'us-west-2\'
describeOfferingResponse_region :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Text)
describeOfferingResponse_region = Lens.lens (\DescribeOfferingResponse' {region} -> region) (\s@DescribeOfferingResponse' {} a -> s {region = a} :: DescribeOfferingResponse)

-- | Resource configuration details
describeOfferingResponse_resourceSpecification :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe ReservationResourceSpecification)
describeOfferingResponse_resourceSpecification = Lens.lens (\DescribeOfferingResponse' {resourceSpecification} -> resourceSpecification) (\s@DescribeOfferingResponse' {} a -> s {resourceSpecification = a} :: DescribeOfferingResponse)

-- | Recurring usage charge for each reserved resource, e.g. \'157.0\'
describeOfferingResponse_usagePrice :: Lens.Lens' DescribeOfferingResponse (Prelude.Maybe Prelude.Double)
describeOfferingResponse_usagePrice = Lens.lens (\DescribeOfferingResponse' {usagePrice} -> usagePrice) (\s@DescribeOfferingResponse' {} a -> s {usagePrice = a} :: DescribeOfferingResponse)

-- | The response's http status code.
describeOfferingResponse_httpStatus :: Lens.Lens' DescribeOfferingResponse Prelude.Int
describeOfferingResponse_httpStatus = Lens.lens (\DescribeOfferingResponse' {httpStatus} -> httpStatus) (\s@DescribeOfferingResponse' {} a -> s {httpStatus = a} :: DescribeOfferingResponse)

instance Prelude.NFData DescribeOfferingResponse where
  rnf DescribeOfferingResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf durationUnits
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf offeringDescription
      `Prelude.seq` Prelude.rnf offeringId
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf resourceSpecification
      `Prelude.seq` Prelude.rnf usagePrice
      `Prelude.seq` Prelude.rnf httpStatus
