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
-- Module      : Network.AWS.ElasticInference.DescribeAcceleratorOfferings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the locations in which a given accelerator type or set of
-- types is present in a given region.
module Network.AWS.ElasticInference.DescribeAcceleratorOfferings
  ( -- * Creating a Request
    DescribeAcceleratorOfferings (..),
    newDescribeAcceleratorOfferings,

    -- * Request Lenses
    describeAcceleratorOfferings_acceleratorTypes,
    describeAcceleratorOfferings_locationType,

    -- * Destructuring the Response
    DescribeAcceleratorOfferingsResponse (..),
    newDescribeAcceleratorOfferingsResponse,

    -- * Response Lenses
    describeAcceleratorOfferingsResponse_acceleratorTypeOfferings,
    describeAcceleratorOfferingsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticInference.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAcceleratorOfferings' smart constructor.
data DescribeAcceleratorOfferings = DescribeAcceleratorOfferings'
  { -- | The list of accelerator types to describe.
    acceleratorTypes :: Prelude.Maybe [Prelude.Text],
    -- | The location type that you want to describe accelerator type offerings
    -- for. It can assume the following values: region: will return the
    -- accelerator type offering at the regional level. availability-zone: will
    -- return the accelerator type offering at the availability zone level.
    -- availability-zone-id: will return the accelerator type offering at the
    -- availability zone level returning the availability zone id.
    locationType :: LocationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAcceleratorOfferings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorTypes', 'describeAcceleratorOfferings_acceleratorTypes' - The list of accelerator types to describe.
--
-- 'locationType', 'describeAcceleratorOfferings_locationType' - The location type that you want to describe accelerator type offerings
-- for. It can assume the following values: region: will return the
-- accelerator type offering at the regional level. availability-zone: will
-- return the accelerator type offering at the availability zone level.
-- availability-zone-id: will return the accelerator type offering at the
-- availability zone level returning the availability zone id.
newDescribeAcceleratorOfferings ::
  -- | 'locationType'
  LocationType ->
  DescribeAcceleratorOfferings
newDescribeAcceleratorOfferings pLocationType_ =
  DescribeAcceleratorOfferings'
    { acceleratorTypes =
        Prelude.Nothing,
      locationType = pLocationType_
    }

-- | The list of accelerator types to describe.
describeAcceleratorOfferings_acceleratorTypes :: Lens.Lens' DescribeAcceleratorOfferings (Prelude.Maybe [Prelude.Text])
describeAcceleratorOfferings_acceleratorTypes = Lens.lens (\DescribeAcceleratorOfferings' {acceleratorTypes} -> acceleratorTypes) (\s@DescribeAcceleratorOfferings' {} a -> s {acceleratorTypes = a} :: DescribeAcceleratorOfferings) Prelude.. Lens.mapping Lens.coerced

-- | The location type that you want to describe accelerator type offerings
-- for. It can assume the following values: region: will return the
-- accelerator type offering at the regional level. availability-zone: will
-- return the accelerator type offering at the availability zone level.
-- availability-zone-id: will return the accelerator type offering at the
-- availability zone level returning the availability zone id.
describeAcceleratorOfferings_locationType :: Lens.Lens' DescribeAcceleratorOfferings LocationType
describeAcceleratorOfferings_locationType = Lens.lens (\DescribeAcceleratorOfferings' {locationType} -> locationType) (\s@DescribeAcceleratorOfferings' {} a -> s {locationType = a} :: DescribeAcceleratorOfferings)

instance Core.AWSRequest DescribeAcceleratorOfferings where
  type
    AWSResponse DescribeAcceleratorOfferings =
      DescribeAcceleratorOfferingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAcceleratorOfferingsResponse'
            Prelude.<$> ( x Core..?> "acceleratorTypeOfferings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAcceleratorOfferings

instance Prelude.NFData DescribeAcceleratorOfferings

instance Core.ToHeaders DescribeAcceleratorOfferings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeAcceleratorOfferings where
  toJSON DescribeAcceleratorOfferings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("acceleratorTypes" Core..=)
              Prelude.<$> acceleratorTypes,
            Prelude.Just ("locationType" Core..= locationType)
          ]
      )

instance Core.ToPath DescribeAcceleratorOfferings where
  toPath =
    Prelude.const "/describe-accelerator-offerings"

instance Core.ToQuery DescribeAcceleratorOfferings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAcceleratorOfferingsResponse' smart constructor.
data DescribeAcceleratorOfferingsResponse = DescribeAcceleratorOfferingsResponse'
  { -- | The list of accelerator type offerings for a specific location.
    acceleratorTypeOfferings :: Prelude.Maybe [AcceleratorTypeOffering],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAcceleratorOfferingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorTypeOfferings', 'describeAcceleratorOfferingsResponse_acceleratorTypeOfferings' - The list of accelerator type offerings for a specific location.
--
-- 'httpStatus', 'describeAcceleratorOfferingsResponse_httpStatus' - The response's http status code.
newDescribeAcceleratorOfferingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAcceleratorOfferingsResponse
newDescribeAcceleratorOfferingsResponse pHttpStatus_ =
  DescribeAcceleratorOfferingsResponse'
    { acceleratorTypeOfferings =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of accelerator type offerings for a specific location.
describeAcceleratorOfferingsResponse_acceleratorTypeOfferings :: Lens.Lens' DescribeAcceleratorOfferingsResponse (Prelude.Maybe [AcceleratorTypeOffering])
describeAcceleratorOfferingsResponse_acceleratorTypeOfferings = Lens.lens (\DescribeAcceleratorOfferingsResponse' {acceleratorTypeOfferings} -> acceleratorTypeOfferings) (\s@DescribeAcceleratorOfferingsResponse' {} a -> s {acceleratorTypeOfferings = a} :: DescribeAcceleratorOfferingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAcceleratorOfferingsResponse_httpStatus :: Lens.Lens' DescribeAcceleratorOfferingsResponse Prelude.Int
describeAcceleratorOfferingsResponse_httpStatus = Lens.lens (\DescribeAcceleratorOfferingsResponse' {httpStatus} -> httpStatus) (\s@DescribeAcceleratorOfferingsResponse' {} a -> s {httpStatus = a} :: DescribeAcceleratorOfferingsResponse)

instance
  Prelude.NFData
    DescribeAcceleratorOfferingsResponse
