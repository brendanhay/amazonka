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
-- Module      : Amazonka.GlobalAccelerator.ListCustomRoutingPortMappingsByDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the port mappings for a specific EC2 instance (destination) in a
-- VPC subnet endpoint. The response is the mappings for one destination IP
-- address. This is useful when your subnet endpoint has mappings that span
-- multiple custom routing accelerators in your account, or for scenarios
-- where you only want to list the port mappings for a specific destination
-- instance.
--
-- This operation returns paginated results.
module Amazonka.GlobalAccelerator.ListCustomRoutingPortMappingsByDestination
  ( -- * Creating a Request
    ListCustomRoutingPortMappingsByDestination (..),
    newListCustomRoutingPortMappingsByDestination,

    -- * Request Lenses
    listCustomRoutingPortMappingsByDestination_maxResults,
    listCustomRoutingPortMappingsByDestination_nextToken,
    listCustomRoutingPortMappingsByDestination_endpointId,
    listCustomRoutingPortMappingsByDestination_destinationAddress,

    -- * Destructuring the Response
    ListCustomRoutingPortMappingsByDestinationResponse (..),
    newListCustomRoutingPortMappingsByDestinationResponse,

    -- * Response Lenses
    listCustomRoutingPortMappingsByDestinationResponse_destinationPortMappings,
    listCustomRoutingPortMappingsByDestinationResponse_nextToken,
    listCustomRoutingPortMappingsByDestinationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomRoutingPortMappingsByDestination' smart constructor.
data ListCustomRoutingPortMappingsByDestination = ListCustomRoutingPortMappingsByDestination'
  { -- | The number of destination port mappings that you want to return with
    -- this call. The default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID for the virtual private cloud (VPC) subnet.
    endpointId :: Prelude.Text,
    -- | The endpoint IP address in a virtual private cloud (VPC) subnet for
    -- which you want to receive back port mappings.
    destinationAddress :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomRoutingPortMappingsByDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCustomRoutingPortMappingsByDestination_maxResults' - The number of destination port mappings that you want to return with
-- this call. The default value is 10.
--
-- 'nextToken', 'listCustomRoutingPortMappingsByDestination_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'endpointId', 'listCustomRoutingPortMappingsByDestination_endpointId' - The ID for the virtual private cloud (VPC) subnet.
--
-- 'destinationAddress', 'listCustomRoutingPortMappingsByDestination_destinationAddress' - The endpoint IP address in a virtual private cloud (VPC) subnet for
-- which you want to receive back port mappings.
newListCustomRoutingPortMappingsByDestination ::
  -- | 'endpointId'
  Prelude.Text ->
  -- | 'destinationAddress'
  Prelude.Text ->
  ListCustomRoutingPortMappingsByDestination
newListCustomRoutingPortMappingsByDestination
  pEndpointId_
  pDestinationAddress_ =
    ListCustomRoutingPortMappingsByDestination'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        endpointId = pEndpointId_,
        destinationAddress =
          pDestinationAddress_
      }

-- | The number of destination port mappings that you want to return with
-- this call. The default value is 10.
listCustomRoutingPortMappingsByDestination_maxResults :: Lens.Lens' ListCustomRoutingPortMappingsByDestination (Prelude.Maybe Prelude.Natural)
listCustomRoutingPortMappingsByDestination_maxResults = Lens.lens (\ListCustomRoutingPortMappingsByDestination' {maxResults} -> maxResults) (\s@ListCustomRoutingPortMappingsByDestination' {} a -> s {maxResults = a} :: ListCustomRoutingPortMappingsByDestination)

-- | The token for the next set of results. You receive this token from a
-- previous call.
listCustomRoutingPortMappingsByDestination_nextToken :: Lens.Lens' ListCustomRoutingPortMappingsByDestination (Prelude.Maybe Prelude.Text)
listCustomRoutingPortMappingsByDestination_nextToken = Lens.lens (\ListCustomRoutingPortMappingsByDestination' {nextToken} -> nextToken) (\s@ListCustomRoutingPortMappingsByDestination' {} a -> s {nextToken = a} :: ListCustomRoutingPortMappingsByDestination)

-- | The ID for the virtual private cloud (VPC) subnet.
listCustomRoutingPortMappingsByDestination_endpointId :: Lens.Lens' ListCustomRoutingPortMappingsByDestination Prelude.Text
listCustomRoutingPortMappingsByDestination_endpointId = Lens.lens (\ListCustomRoutingPortMappingsByDestination' {endpointId} -> endpointId) (\s@ListCustomRoutingPortMappingsByDestination' {} a -> s {endpointId = a} :: ListCustomRoutingPortMappingsByDestination)

-- | The endpoint IP address in a virtual private cloud (VPC) subnet for
-- which you want to receive back port mappings.
listCustomRoutingPortMappingsByDestination_destinationAddress :: Lens.Lens' ListCustomRoutingPortMappingsByDestination Prelude.Text
listCustomRoutingPortMappingsByDestination_destinationAddress = Lens.lens (\ListCustomRoutingPortMappingsByDestination' {destinationAddress} -> destinationAddress) (\s@ListCustomRoutingPortMappingsByDestination' {} a -> s {destinationAddress = a} :: ListCustomRoutingPortMappingsByDestination)

instance
  Core.AWSPager
    ListCustomRoutingPortMappingsByDestination
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomRoutingPortMappingsByDestinationResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomRoutingPortMappingsByDestinationResponse_destinationPortMappings
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCustomRoutingPortMappingsByDestination_nextToken
          Lens..~ rs
          Lens.^? listCustomRoutingPortMappingsByDestinationResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListCustomRoutingPortMappingsByDestination
  where
  type
    AWSResponse
      ListCustomRoutingPortMappingsByDestination =
      ListCustomRoutingPortMappingsByDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomRoutingPortMappingsByDestinationResponse'
            Prelude.<$> ( x
                            Data..?> "DestinationPortMappings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListCustomRoutingPortMappingsByDestination
  where
  hashWithSalt
    _salt
    ListCustomRoutingPortMappingsByDestination' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` endpointId
        `Prelude.hashWithSalt` destinationAddress

instance
  Prelude.NFData
    ListCustomRoutingPortMappingsByDestination
  where
  rnf ListCustomRoutingPortMappingsByDestination' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf destinationAddress

instance
  Data.ToHeaders
    ListCustomRoutingPortMappingsByDestination
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GlobalAccelerator_V20180706.ListCustomRoutingPortMappingsByDestination" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListCustomRoutingPortMappingsByDestination
  where
  toJSON
    ListCustomRoutingPortMappingsByDestination' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("MaxResults" Data..=) Prelude.<$> maxResults,
              ("NextToken" Data..=) Prelude.<$> nextToken,
              Prelude.Just ("EndpointId" Data..= endpointId),
              Prelude.Just
                ("DestinationAddress" Data..= destinationAddress)
            ]
        )

instance
  Data.ToPath
    ListCustomRoutingPortMappingsByDestination
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListCustomRoutingPortMappingsByDestination
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomRoutingPortMappingsByDestinationResponse' smart constructor.
data ListCustomRoutingPortMappingsByDestinationResponse = ListCustomRoutingPortMappingsByDestinationResponse'
  { -- | The port mappings for the endpoint IP address that you specified in the
    -- request.
    destinationPortMappings :: Prelude.Maybe [DestinationPortMapping],
    -- | The token for the next set of results. You receive this token from a
    -- previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomRoutingPortMappingsByDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationPortMappings', 'listCustomRoutingPortMappingsByDestinationResponse_destinationPortMappings' - The port mappings for the endpoint IP address that you specified in the
-- request.
--
-- 'nextToken', 'listCustomRoutingPortMappingsByDestinationResponse_nextToken' - The token for the next set of results. You receive this token from a
-- previous call.
--
-- 'httpStatus', 'listCustomRoutingPortMappingsByDestinationResponse_httpStatus' - The response's http status code.
newListCustomRoutingPortMappingsByDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomRoutingPortMappingsByDestinationResponse
newListCustomRoutingPortMappingsByDestinationResponse
  pHttpStatus_ =
    ListCustomRoutingPortMappingsByDestinationResponse'
      { destinationPortMappings =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The port mappings for the endpoint IP address that you specified in the
-- request.
listCustomRoutingPortMappingsByDestinationResponse_destinationPortMappings :: Lens.Lens' ListCustomRoutingPortMappingsByDestinationResponse (Prelude.Maybe [DestinationPortMapping])
listCustomRoutingPortMappingsByDestinationResponse_destinationPortMappings = Lens.lens (\ListCustomRoutingPortMappingsByDestinationResponse' {destinationPortMappings} -> destinationPortMappings) (\s@ListCustomRoutingPortMappingsByDestinationResponse' {} a -> s {destinationPortMappings = a} :: ListCustomRoutingPortMappingsByDestinationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results. You receive this token from a
-- previous call.
listCustomRoutingPortMappingsByDestinationResponse_nextToken :: Lens.Lens' ListCustomRoutingPortMappingsByDestinationResponse (Prelude.Maybe Prelude.Text)
listCustomRoutingPortMappingsByDestinationResponse_nextToken = Lens.lens (\ListCustomRoutingPortMappingsByDestinationResponse' {nextToken} -> nextToken) (\s@ListCustomRoutingPortMappingsByDestinationResponse' {} a -> s {nextToken = a} :: ListCustomRoutingPortMappingsByDestinationResponse)

-- | The response's http status code.
listCustomRoutingPortMappingsByDestinationResponse_httpStatus :: Lens.Lens' ListCustomRoutingPortMappingsByDestinationResponse Prelude.Int
listCustomRoutingPortMappingsByDestinationResponse_httpStatus = Lens.lens (\ListCustomRoutingPortMappingsByDestinationResponse' {httpStatus} -> httpStatus) (\s@ListCustomRoutingPortMappingsByDestinationResponse' {} a -> s {httpStatus = a} :: ListCustomRoutingPortMappingsByDestinationResponse)

instance
  Prelude.NFData
    ListCustomRoutingPortMappingsByDestinationResponse
  where
  rnf
    ListCustomRoutingPortMappingsByDestinationResponse' {..} =
      Prelude.rnf destinationPortMappings
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
