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
-- Module      : Amazonka.GlobalAccelerator.ListCustomRoutingPortMappings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a complete mapping from the public accelerator IP address and
-- port to destination EC2 instance IP addresses and ports in the virtual
-- public cloud (VPC) subnet endpoint for a custom routing accelerator. For
-- each subnet endpoint that you add, Global Accelerator creates a new
-- static port mapping for the accelerator. The port mappings don\'t change
-- after Global Accelerator generates them, so you can retrieve and cache
-- the full mapping on your servers.
--
-- If you remove a subnet from your accelerator, Global Accelerator removes
-- (reclaims) the port mappings. If you add a subnet to your accelerator,
-- Global Accelerator creates new port mappings (the existing ones don\'t
-- change). If you add or remove EC2 instances in your subnet, the port
-- mappings don\'t change, because the mappings are created when you add
-- the subnet to Global Accelerator.
--
-- The mappings also include a flag for each destination denoting which
-- destination IP addresses and ports are allowed or denied traffic.
--
-- This operation returns paginated results.
module Amazonka.GlobalAccelerator.ListCustomRoutingPortMappings
  ( -- * Creating a Request
    ListCustomRoutingPortMappings (..),
    newListCustomRoutingPortMappings,

    -- * Request Lenses
    listCustomRoutingPortMappings_endpointGroupArn,
    listCustomRoutingPortMappings_maxResults,
    listCustomRoutingPortMappings_nextToken,
    listCustomRoutingPortMappings_acceleratorArn,

    -- * Destructuring the Response
    ListCustomRoutingPortMappingsResponse (..),
    newListCustomRoutingPortMappingsResponse,

    -- * Response Lenses
    listCustomRoutingPortMappingsResponse_nextToken,
    listCustomRoutingPortMappingsResponse_portMappings,
    listCustomRoutingPortMappingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomRoutingPortMappings' smart constructor.
data ListCustomRoutingPortMappings = ListCustomRoutingPortMappings'
  { -- | The Amazon Resource Name (ARN) of the endpoint group to list the custom
    -- routing port mappings for.
    endpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The number of destination port mappings that you want to return with
    -- this call. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the accelerator to list the custom
    -- routing port mappings for.
    acceleratorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomRoutingPortMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointGroupArn', 'listCustomRoutingPortMappings_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group to list the custom
-- routing port mappings for.
--
-- 'maxResults', 'listCustomRoutingPortMappings_maxResults' - The number of destination port mappings that you want to return with
-- this call. The default value is 10.
--
-- 'nextToken', 'listCustomRoutingPortMappings_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'acceleratorArn', 'listCustomRoutingPortMappings_acceleratorArn' - The Amazon Resource Name (ARN) of the accelerator to list the custom
-- routing port mappings for.
newListCustomRoutingPortMappings ::
  -- | 'acceleratorArn'
  Prelude.Text ->
  ListCustomRoutingPortMappings
newListCustomRoutingPortMappings pAcceleratorArn_ =
  ListCustomRoutingPortMappings'
    { endpointGroupArn =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      acceleratorArn = pAcceleratorArn_
    }

-- | The Amazon Resource Name (ARN) of the endpoint group to list the custom
-- routing port mappings for.
listCustomRoutingPortMappings_endpointGroupArn :: Lens.Lens' ListCustomRoutingPortMappings (Prelude.Maybe Prelude.Text)
listCustomRoutingPortMappings_endpointGroupArn = Lens.lens (\ListCustomRoutingPortMappings' {endpointGroupArn} -> endpointGroupArn) (\s@ListCustomRoutingPortMappings' {} a -> s {endpointGroupArn = a} :: ListCustomRoutingPortMappings)

-- | The number of destination port mappings that you want to return with
-- this call. The default value is 10.
listCustomRoutingPortMappings_maxResults :: Lens.Lens' ListCustomRoutingPortMappings (Prelude.Maybe Prelude.Natural)
listCustomRoutingPortMappings_maxResults = Lens.lens (\ListCustomRoutingPortMappings' {maxResults} -> maxResults) (\s@ListCustomRoutingPortMappings' {} a -> s {maxResults = a} :: ListCustomRoutingPortMappings)

-- | The token for the next set of results. You receive this token from a
-- previous call.
listCustomRoutingPortMappings_nextToken :: Lens.Lens' ListCustomRoutingPortMappings (Prelude.Maybe Prelude.Text)
listCustomRoutingPortMappings_nextToken = Lens.lens (\ListCustomRoutingPortMappings' {nextToken} -> nextToken) (\s@ListCustomRoutingPortMappings' {} a -> s {nextToken = a} :: ListCustomRoutingPortMappings)

-- | The Amazon Resource Name (ARN) of the accelerator to list the custom
-- routing port mappings for.
listCustomRoutingPortMappings_acceleratorArn :: Lens.Lens' ListCustomRoutingPortMappings Prelude.Text
listCustomRoutingPortMappings_acceleratorArn = Lens.lens (\ListCustomRoutingPortMappings' {acceleratorArn} -> acceleratorArn) (\s@ListCustomRoutingPortMappings' {} a -> s {acceleratorArn = a} :: ListCustomRoutingPortMappings)

instance Core.AWSPager ListCustomRoutingPortMappings where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomRoutingPortMappingsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomRoutingPortMappingsResponse_portMappings
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCustomRoutingPortMappings_nextToken
          Lens..~ rs
          Lens.^? listCustomRoutingPortMappingsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListCustomRoutingPortMappings
  where
  type
    AWSResponse ListCustomRoutingPortMappings =
      ListCustomRoutingPortMappingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomRoutingPortMappingsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "PortMappings" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCustomRoutingPortMappings
  where
  hashWithSalt _salt ListCustomRoutingPortMappings' {..} =
    _salt
      `Prelude.hashWithSalt` endpointGroupArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` acceleratorArn

instance Prelude.NFData ListCustomRoutingPortMappings where
  rnf ListCustomRoutingPortMappings' {..} =
    Prelude.rnf endpointGroupArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf acceleratorArn

instance Data.ToHeaders ListCustomRoutingPortMappings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.ListCustomRoutingPortMappings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCustomRoutingPortMappings where
  toJSON ListCustomRoutingPortMappings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndpointGroupArn" Data..=)
              Prelude.<$> endpointGroupArn,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("AcceleratorArn" Data..= acceleratorArn)
          ]
      )

instance Data.ToPath ListCustomRoutingPortMappings where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCustomRoutingPortMappings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomRoutingPortMappingsResponse' smart constructor.
data ListCustomRoutingPortMappingsResponse = ListCustomRoutingPortMappingsResponse'
  { -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The port mappings for a custom routing accelerator.
    portMappings :: Prelude.Maybe [PortMapping],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomRoutingPortMappingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomRoutingPortMappingsResponse_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'portMappings', 'listCustomRoutingPortMappingsResponse_portMappings' - The port mappings for a custom routing accelerator.
--
-- 'httpStatus', 'listCustomRoutingPortMappingsResponse_httpStatus' - The response's http status code.
newListCustomRoutingPortMappingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomRoutingPortMappingsResponse
newListCustomRoutingPortMappingsResponse pHttpStatus_ =
  ListCustomRoutingPortMappingsResponse'
    { nextToken =
        Prelude.Nothing,
      portMappings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results. You receive this token from a
-- previous call.
listCustomRoutingPortMappingsResponse_nextToken :: Lens.Lens' ListCustomRoutingPortMappingsResponse (Prelude.Maybe Prelude.Text)
listCustomRoutingPortMappingsResponse_nextToken = Lens.lens (\ListCustomRoutingPortMappingsResponse' {nextToken} -> nextToken) (\s@ListCustomRoutingPortMappingsResponse' {} a -> s {nextToken = a} :: ListCustomRoutingPortMappingsResponse)

-- | The port mappings for a custom routing accelerator.
listCustomRoutingPortMappingsResponse_portMappings :: Lens.Lens' ListCustomRoutingPortMappingsResponse (Prelude.Maybe [PortMapping])
listCustomRoutingPortMappingsResponse_portMappings = Lens.lens (\ListCustomRoutingPortMappingsResponse' {portMappings} -> portMappings) (\s@ListCustomRoutingPortMappingsResponse' {} a -> s {portMappings = a} :: ListCustomRoutingPortMappingsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCustomRoutingPortMappingsResponse_httpStatus :: Lens.Lens' ListCustomRoutingPortMappingsResponse Prelude.Int
listCustomRoutingPortMappingsResponse_httpStatus = Lens.lens (\ListCustomRoutingPortMappingsResponse' {httpStatus} -> httpStatus) (\s@ListCustomRoutingPortMappingsResponse' {} a -> s {httpStatus = a} :: ListCustomRoutingPortMappingsResponse)

instance
  Prelude.NFData
    ListCustomRoutingPortMappingsResponse
  where
  rnf ListCustomRoutingPortMappingsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf portMappings
      `Prelude.seq` Prelude.rnf httpStatus
