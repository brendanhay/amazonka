{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.DescribeEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a unique endpoint specific to the AWS account making the call.
module Network.AWS.IoT.DescribeEndpoint
  ( -- * Creating a Request
    DescribeEndpoint (..),
    newDescribeEndpoint,

    -- * Request Lenses
    describeEndpoint_endpointType,

    -- * Destructuring the Response
    DescribeEndpointResponse (..),
    newDescribeEndpointResponse,

    -- * Response Lenses
    describeEndpointResponse_endpointAddress,
    describeEndpointResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeEndpoint operation.
--
-- /See:/ 'newDescribeEndpoint' smart constructor.
data DescribeEndpoint = DescribeEndpoint'
  { -- | The endpoint type. Valid endpoint types include:
    --
    -- -   @iot:Data@ - Returns a VeriSign signed data endpoint.
    --
    -- -   @iot:Data-ATS@ - Returns an ATS signed data endpoint.
    --
    -- -   @iot:CredentialProvider@ - Returns an AWS IoT credentials provider
    --     API endpoint.
    --
    -- -   @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
    --
    -- We strongly recommend that customers use the newer @iot:Data-ATS@
    -- endpoint type to avoid issues related to the widespread distrust of
    -- Symantec certificate authorities.
    endpointType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointType', 'describeEndpoint_endpointType' - The endpoint type. Valid endpoint types include:
--
-- -   @iot:Data@ - Returns a VeriSign signed data endpoint.
--
-- -   @iot:Data-ATS@ - Returns an ATS signed data endpoint.
--
-- -   @iot:CredentialProvider@ - Returns an AWS IoT credentials provider
--     API endpoint.
--
-- -   @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
--
-- We strongly recommend that customers use the newer @iot:Data-ATS@
-- endpoint type to avoid issues related to the widespread distrust of
-- Symantec certificate authorities.
newDescribeEndpoint ::
  DescribeEndpoint
newDescribeEndpoint =
  DescribeEndpoint' {endpointType = Prelude.Nothing}

-- | The endpoint type. Valid endpoint types include:
--
-- -   @iot:Data@ - Returns a VeriSign signed data endpoint.
--
-- -   @iot:Data-ATS@ - Returns an ATS signed data endpoint.
--
-- -   @iot:CredentialProvider@ - Returns an AWS IoT credentials provider
--     API endpoint.
--
-- -   @iot:Jobs@ - Returns an AWS IoT device management Jobs API endpoint.
--
-- We strongly recommend that customers use the newer @iot:Data-ATS@
-- endpoint type to avoid issues related to the widespread distrust of
-- Symantec certificate authorities.
describeEndpoint_endpointType :: Lens.Lens' DescribeEndpoint (Prelude.Maybe Prelude.Text)
describeEndpoint_endpointType = Lens.lens (\DescribeEndpoint' {endpointType} -> endpointType) (\s@DescribeEndpoint' {} a -> s {endpointType = a} :: DescribeEndpoint)

instance Prelude.AWSRequest DescribeEndpoint where
  type Rs DescribeEndpoint = DescribeEndpointResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Prelude.<$> (x Prelude..?> "endpointAddress")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpoint

instance Prelude.NFData DescribeEndpoint

instance Prelude.ToHeaders DescribeEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeEndpoint where
  toPath = Prelude.const "/endpoint"

instance Prelude.ToQuery DescribeEndpoint where
  toQuery DescribeEndpoint' {..} =
    Prelude.mconcat
      ["endpointType" Prelude.=: endpointType]

-- | The output from the DescribeEndpoint operation.
--
-- /See:/ 'newDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { -- | The endpoint. The format of the endpoint is as follows:
    -- /identifier/.iot./region/.amazonaws.com.
    endpointAddress :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointAddress', 'describeEndpointResponse_endpointAddress' - The endpoint. The format of the endpoint is as follows:
-- /identifier/.iot./region/.amazonaws.com.
--
-- 'httpStatus', 'describeEndpointResponse_httpStatus' - The response's http status code.
newDescribeEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointResponse
newDescribeEndpointResponse pHttpStatus_ =
  DescribeEndpointResponse'
    { endpointAddress =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint. The format of the endpoint is as follows:
-- /identifier/.iot./region/.amazonaws.com.
describeEndpointResponse_endpointAddress :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.Text)
describeEndpointResponse_endpointAddress = Lens.lens (\DescribeEndpointResponse' {endpointAddress} -> endpointAddress) (\s@DescribeEndpointResponse' {} a -> s {endpointAddress = a} :: DescribeEndpointResponse)

-- | The response's http status code.
describeEndpointResponse_httpStatus :: Lens.Lens' DescribeEndpointResponse Prelude.Int
describeEndpointResponse_httpStatus = Lens.lens (\DescribeEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointResponse' {} a -> s {httpStatus = a} :: DescribeEndpointResponse)

instance Prelude.NFData DescribeEndpointResponse
