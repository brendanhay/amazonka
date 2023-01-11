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
-- Module      : Amazonka.DirectConnect.DescribeLocations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Direct Connect locations in the current Amazon Web Services
-- Region. These are the locations that can be selected when calling
-- CreateConnection or CreateInterconnect.
module Amazonka.DirectConnect.DescribeLocations
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLocations' smart constructor.
data DescribeLocations = DescribeLocations'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationsResponse'
            Prelude.<$> (x Data..?> "locations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLocations where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeLocations where
  rnf _ = ()

instance Data.ToHeaders DescribeLocations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DescribeLocations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLocations where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath DescribeLocations where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLocations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLocationsResponse' smart constructor.
data DescribeLocationsResponse = DescribeLocationsResponse'
  { -- | The locations.
    locations :: Prelude.Maybe [Location],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeLocationsResponse
newDescribeLocationsResponse pHttpStatus_ =
  DescribeLocationsResponse'
    { locations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The locations.
describeLocationsResponse_locations :: Lens.Lens' DescribeLocationsResponse (Prelude.Maybe [Location])
describeLocationsResponse_locations = Lens.lens (\DescribeLocationsResponse' {locations} -> locations) (\s@DescribeLocationsResponse' {} a -> s {locations = a} :: DescribeLocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLocationsResponse_httpStatus :: Lens.Lens' DescribeLocationsResponse Prelude.Int
describeLocationsResponse_httpStatus = Lens.lens (\DescribeLocationsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationsResponse' {} a -> s {httpStatus = a} :: DescribeLocationsResponse)

instance Prelude.NFData DescribeLocationsResponse where
  rnf DescribeLocationsResponse' {..} =
    Prelude.rnf locations
      `Prelude.seq` Prelude.rnf httpStatus
