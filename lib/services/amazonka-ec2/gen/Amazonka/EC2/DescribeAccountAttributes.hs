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
-- Module      : Amazonka.EC2.DescribeAccountAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes attributes of your Amazon Web Services account. The following
-- are the supported account attributes:
--
-- -   @supported-platforms@: Indicates whether your account can launch
--     instances into EC2-Classic and EC2-VPC, or only into EC2-VPC.
--
-- -   @default-vpc@: The ID of the default VPC for your account, or
--     @none@.
--
-- -   @max-instances@: This attribute is no longer supported. The returned
--     value does not reflect your actual vCPU limit for running On-Demand
--     Instances. For more information, see
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-on-demand-instances.html#ec2-on-demand-instances-limits On-Demand Instance Limits>
--     in the /Amazon Elastic Compute Cloud User Guide/.
--
-- -   @vpc-max-security-groups-per-interface@: The maximum number of
--     security groups that you can assign to a network interface.
--
-- -   @max-elastic-ips@: The maximum number of Elastic IP addresses that
--     you can allocate for use with EC2-Classic.
--
-- -   @vpc-max-elastic-ips@: The maximum number of Elastic IP addresses
--     that you can allocate for use with EC2-VPC.
--
-- We are retiring EC2-Classic on August 15, 2022. We recommend that you
-- migrate from EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.DescribeAccountAttributes
  ( -- * Creating a Request
    DescribeAccountAttributes (..),
    newDescribeAccountAttributes,

    -- * Request Lenses
    describeAccountAttributes_attributeNames,
    describeAccountAttributes_dryRun,

    -- * Destructuring the Response
    DescribeAccountAttributesResponse (..),
    newDescribeAccountAttributesResponse,

    -- * Response Lenses
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAccountAttributes' smart constructor.
data DescribeAccountAttributes = DescribeAccountAttributes'
  { -- | The account attribute names.
    attributeNames :: Prelude.Maybe [AccountAttributeName],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeNames', 'describeAccountAttributes_attributeNames' - The account attribute names.
--
-- 'dryRun', 'describeAccountAttributes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDescribeAccountAttributes ::
  DescribeAccountAttributes
newDescribeAccountAttributes =
  DescribeAccountAttributes'
    { attributeNames =
        Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | The account attribute names.
describeAccountAttributes_attributeNames :: Lens.Lens' DescribeAccountAttributes (Prelude.Maybe [AccountAttributeName])
describeAccountAttributes_attributeNames = Lens.lens (\DescribeAccountAttributes' {attributeNames} -> attributeNames) (\s@DescribeAccountAttributes' {} a -> s {attributeNames = a} :: DescribeAccountAttributes) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeAccountAttributes_dryRun :: Lens.Lens' DescribeAccountAttributes (Prelude.Maybe Prelude.Bool)
describeAccountAttributes_dryRun = Lens.lens (\DescribeAccountAttributes' {dryRun} -> dryRun) (\s@DescribeAccountAttributes' {} a -> s {dryRun = a} :: DescribeAccountAttributes)

instance Core.AWSRequest DescribeAccountAttributes where
  type
    AWSResponse DescribeAccountAttributes =
      DescribeAccountAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeAccountAttributesResponse'
            Prelude.<$> ( x
                            Data..@? "accountAttributeSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeAccountAttributes where
  hashWithSalt _salt DescribeAccountAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` attributeNames
      `Prelude.hashWithSalt` dryRun

instance Prelude.NFData DescribeAccountAttributes where
  rnf DescribeAccountAttributes' {..} =
    Prelude.rnf attributeNames
      `Prelude.seq` Prelude.rnf dryRun

instance Data.ToHeaders DescribeAccountAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAccountAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeAccountAttributes where
  toQuery DescribeAccountAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeAccountAttributes" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          ( Data.toQueryList "AttributeName"
              Prelude.<$> attributeNames
          ),
        "DryRun" Data.=: dryRun
      ]

-- | /See:/ 'newDescribeAccountAttributesResponse' smart constructor.
data DescribeAccountAttributesResponse = DescribeAccountAttributesResponse'
  { -- | Information about the account attributes.
    accountAttributes :: Prelude.Maybe [AccountAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAccountAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountAttributes', 'describeAccountAttributesResponse_accountAttributes' - Information about the account attributes.
--
-- 'httpStatus', 'describeAccountAttributesResponse_httpStatus' - The response's http status code.
newDescribeAccountAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAccountAttributesResponse
newDescribeAccountAttributesResponse pHttpStatus_ =
  DescribeAccountAttributesResponse'
    { accountAttributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the account attributes.
describeAccountAttributesResponse_accountAttributes :: Lens.Lens' DescribeAccountAttributesResponse (Prelude.Maybe [AccountAttribute])
describeAccountAttributesResponse_accountAttributes = Lens.lens (\DescribeAccountAttributesResponse' {accountAttributes} -> accountAttributes) (\s@DescribeAccountAttributesResponse' {} a -> s {accountAttributes = a} :: DescribeAccountAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeAccountAttributesResponse_httpStatus :: Lens.Lens' DescribeAccountAttributesResponse Prelude.Int
describeAccountAttributesResponse_httpStatus = Lens.lens (\DescribeAccountAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeAccountAttributesResponse' {} a -> s {httpStatus = a} :: DescribeAccountAttributesResponse)

instance
  Prelude.NFData
    DescribeAccountAttributesResponse
  where
  rnf DescribeAccountAttributesResponse' {..} =
    Prelude.rnf accountAttributes
      `Prelude.seq` Prelude.rnf httpStatus
