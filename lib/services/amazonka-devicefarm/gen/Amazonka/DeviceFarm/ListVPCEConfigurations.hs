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
-- Module      : Amazonka.DeviceFarm.ListVPCEConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all Amazon Virtual Private Cloud (VPC)
-- endpoint configurations in the AWS account.
--
-- This operation returns paginated results.
module Amazonka.DeviceFarm.ListVPCEConfigurations
  ( -- * Creating a Request
    ListVPCEConfigurations (..),
    newListVPCEConfigurations,

    -- * Request Lenses
    listVPCEConfigurations_nextToken,
    listVPCEConfigurations_maxResults,

    -- * Destructuring the Response
    ListVPCEConfigurationsResponse (..),
    newListVPCEConfigurationsResponse,

    -- * Response Lenses
    listVPCEConfigurationsResponse_nextToken,
    listVPCEConfigurationsResponse_vpceConfigurations,
    listVPCEConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVPCEConfigurations' smart constructor.
data ListVPCEConfigurations = ListVPCEConfigurations'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An integer that specifies the maximum number of items you want to return
    -- in the API response.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVPCEConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVPCEConfigurations_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listVPCEConfigurations_maxResults' - An integer that specifies the maximum number of items you want to return
-- in the API response.
newListVPCEConfigurations ::
  ListVPCEConfigurations
newListVPCEConfigurations =
  ListVPCEConfigurations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listVPCEConfigurations_nextToken :: Lens.Lens' ListVPCEConfigurations (Prelude.Maybe Prelude.Text)
listVPCEConfigurations_nextToken = Lens.lens (\ListVPCEConfigurations' {nextToken} -> nextToken) (\s@ListVPCEConfigurations' {} a -> s {nextToken = a} :: ListVPCEConfigurations)

-- | An integer that specifies the maximum number of items you want to return
-- in the API response.
listVPCEConfigurations_maxResults :: Lens.Lens' ListVPCEConfigurations (Prelude.Maybe Prelude.Int)
listVPCEConfigurations_maxResults = Lens.lens (\ListVPCEConfigurations' {maxResults} -> maxResults) (\s@ListVPCEConfigurations' {} a -> s {maxResults = a} :: ListVPCEConfigurations)

instance Core.AWSPager ListVPCEConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVPCEConfigurationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listVPCEConfigurationsResponse_vpceConfigurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listVPCEConfigurations_nextToken
          Lens..~ rs
          Lens.^? listVPCEConfigurationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListVPCEConfigurations where
  type
    AWSResponse ListVPCEConfigurations =
      ListVPCEConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVPCEConfigurationsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "vpceConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVPCEConfigurations where
  hashWithSalt _salt ListVPCEConfigurations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListVPCEConfigurations where
  rnf ListVPCEConfigurations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListVPCEConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListVPCEConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListVPCEConfigurations where
  toJSON ListVPCEConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListVPCEConfigurations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListVPCEConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVPCEConfigurationsResponse' smart constructor.
data ListVPCEConfigurationsResponse = ListVPCEConfigurationsResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @VPCEConfiguration@ objects that contain information about
    -- your VPC endpoint configuration.
    vpceConfigurations :: Prelude.Maybe [VPCEConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVPCEConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVPCEConfigurationsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'vpceConfigurations', 'listVPCEConfigurationsResponse_vpceConfigurations' - An array of @VPCEConfiguration@ objects that contain information about
-- your VPC endpoint configuration.
--
-- 'httpStatus', 'listVPCEConfigurationsResponse_httpStatus' - The response's http status code.
newListVPCEConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVPCEConfigurationsResponse
newListVPCEConfigurationsResponse pHttpStatus_ =
  ListVPCEConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      vpceConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listVPCEConfigurationsResponse_nextToken :: Lens.Lens' ListVPCEConfigurationsResponse (Prelude.Maybe Prelude.Text)
listVPCEConfigurationsResponse_nextToken = Lens.lens (\ListVPCEConfigurationsResponse' {nextToken} -> nextToken) (\s@ListVPCEConfigurationsResponse' {} a -> s {nextToken = a} :: ListVPCEConfigurationsResponse)

-- | An array of @VPCEConfiguration@ objects that contain information about
-- your VPC endpoint configuration.
listVPCEConfigurationsResponse_vpceConfigurations :: Lens.Lens' ListVPCEConfigurationsResponse (Prelude.Maybe [VPCEConfiguration])
listVPCEConfigurationsResponse_vpceConfigurations = Lens.lens (\ListVPCEConfigurationsResponse' {vpceConfigurations} -> vpceConfigurations) (\s@ListVPCEConfigurationsResponse' {} a -> s {vpceConfigurations = a} :: ListVPCEConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVPCEConfigurationsResponse_httpStatus :: Lens.Lens' ListVPCEConfigurationsResponse Prelude.Int
listVPCEConfigurationsResponse_httpStatus = Lens.lens (\ListVPCEConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListVPCEConfigurationsResponse' {} a -> s {httpStatus = a} :: ListVPCEConfigurationsResponse)

instance
  Prelude.NFData
    ListVPCEConfigurationsResponse
  where
  rnf ListVPCEConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpceConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
