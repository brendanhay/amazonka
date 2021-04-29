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
-- Module      : Network.AWS.CloudSearch.DescribeScalingParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the scaling parameters configured for a domain. A domain\'s scaling
-- parameters specify the desired search instance type and replication
-- count. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-scaling-options.html Configuring Scaling Options>
-- in the /Amazon CloudSearch Developer Guide/.
module Network.AWS.CloudSearch.DescribeScalingParameters
  ( -- * Creating a Request
    DescribeScalingParameters (..),
    newDescribeScalingParameters,

    -- * Request Lenses
    describeScalingParameters_domainName,

    -- * Destructuring the Response
    DescribeScalingParametersResponse (..),
    newDescribeScalingParametersResponse,

    -- * Response Lenses
    describeScalingParametersResponse_httpStatus,
    describeScalingParametersResponse_scalingParameters,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DescribeScalingParameters@
-- operation. Specifies the name of the domain you want to describe.
--
-- /See:/ 'newDescribeScalingParameters' smart constructor.
data DescribeScalingParameters = DescribeScalingParameters'
  { domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'describeScalingParameters_domainName' - Undocumented member.
newDescribeScalingParameters ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeScalingParameters
newDescribeScalingParameters pDomainName_ =
  DescribeScalingParameters'
    { domainName =
        pDomainName_
    }

-- | Undocumented member.
describeScalingParameters_domainName :: Lens.Lens' DescribeScalingParameters Prelude.Text
describeScalingParameters_domainName = Lens.lens (\DescribeScalingParameters' {domainName} -> domainName) (\s@DescribeScalingParameters' {} a -> s {domainName = a} :: DescribeScalingParameters)

instance Prelude.AWSRequest DescribeScalingParameters where
  type
    Rs DescribeScalingParameters =
      DescribeScalingParametersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeScalingParametersResult"
      ( \s h x ->
          DescribeScalingParametersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "ScalingParameters")
      )

instance Prelude.Hashable DescribeScalingParameters

instance Prelude.NFData DescribeScalingParameters

instance Prelude.ToHeaders DescribeScalingParameters where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeScalingParameters where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeScalingParameters where
  toQuery DescribeScalingParameters' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DescribeScalingParameters" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Prelude.=: domainName
      ]

-- | The result of a @DescribeScalingParameters@ request. Contains the
-- scaling parameters configured for the domain specified in the request.
--
-- /See:/ 'newDescribeScalingParametersResponse' smart constructor.
data DescribeScalingParametersResponse = DescribeScalingParametersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    scalingParameters :: ScalingParametersStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeScalingParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeScalingParametersResponse_httpStatus' - The response's http status code.
--
-- 'scalingParameters', 'describeScalingParametersResponse_scalingParameters' - Undocumented member.
newDescribeScalingParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'scalingParameters'
  ScalingParametersStatus ->
  DescribeScalingParametersResponse
newDescribeScalingParametersResponse
  pHttpStatus_
  pScalingParameters_ =
    DescribeScalingParametersResponse'
      { httpStatus =
          pHttpStatus_,
        scalingParameters = pScalingParameters_
      }

-- | The response's http status code.
describeScalingParametersResponse_httpStatus :: Lens.Lens' DescribeScalingParametersResponse Prelude.Int
describeScalingParametersResponse_httpStatus = Lens.lens (\DescribeScalingParametersResponse' {httpStatus} -> httpStatus) (\s@DescribeScalingParametersResponse' {} a -> s {httpStatus = a} :: DescribeScalingParametersResponse)

-- | Undocumented member.
describeScalingParametersResponse_scalingParameters :: Lens.Lens' DescribeScalingParametersResponse ScalingParametersStatus
describeScalingParametersResponse_scalingParameters = Lens.lens (\DescribeScalingParametersResponse' {scalingParameters} -> scalingParameters) (\s@DescribeScalingParametersResponse' {} a -> s {scalingParameters = a} :: DescribeScalingParametersResponse)

instance
  Prelude.NFData
    DescribeScalingParametersResponse
