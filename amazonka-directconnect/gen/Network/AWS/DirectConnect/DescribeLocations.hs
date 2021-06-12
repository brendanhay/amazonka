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
-- Module      : Network.AWS.DirectConnect.DescribeLocations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AWS Direct Connect locations in the current AWS Region. These
-- are the locations that can be selected when calling CreateConnection or
-- CreateInterconnect.
module Network.AWS.DirectConnect.DescribeLocations
  ( -- * Creating a Request
    DescribeLocations (..),
    newDescribeLocations,

    -- * Destructuring the Response
    DescribeLocationsResponse (..),
    newDescribeLocationsResponse,

    -- * Response Lenses
    describeLocationsResponse_locations,
    describeLocationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLocations' smart constructor.
data DescribeLocations = DescribeLocations'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeLocations ::
  DescribeLocations
newDescribeLocations = DescribeLocations'

instance Core.AWSRequest DescribeLocations where
  type
    AWSResponse DescribeLocations =
      DescribeLocationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationsResponse'
            Core.<$> (x Core..?> "locations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLocations

instance Core.NFData DescribeLocations

instance Core.ToHeaders DescribeLocations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeLocations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeLocations where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DescribeLocations where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLocations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeLocationsResponse' smart constructor.
data DescribeLocationsResponse = DescribeLocationsResponse'
  { -- | The locations.
    locations :: Core.Maybe [Location],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLocationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locations', 'describeLocationsResponse_locations' - The locations.
--
-- 'httpStatus', 'describeLocationsResponse_httpStatus' - The response's http status code.
newDescribeLocationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLocationsResponse
newDescribeLocationsResponse pHttpStatus_ =
  DescribeLocationsResponse'
    { locations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The locations.
describeLocationsResponse_locations :: Lens.Lens' DescribeLocationsResponse (Core.Maybe [Location])
describeLocationsResponse_locations = Lens.lens (\DescribeLocationsResponse' {locations} -> locations) (\s@DescribeLocationsResponse' {} a -> s {locations = a} :: DescribeLocationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLocationsResponse_httpStatus :: Lens.Lens' DescribeLocationsResponse Core.Int
describeLocationsResponse_httpStatus = Lens.lens (\DescribeLocationsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationsResponse' {} a -> s {httpStatus = a} :: DescribeLocationsResponse)

instance Core.NFData DescribeLocationsResponse
