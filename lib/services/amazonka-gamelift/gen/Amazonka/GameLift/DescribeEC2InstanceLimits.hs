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
-- Module      : Amazonka.GameLift.DescribeEC2InstanceLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the instance limits and current utilization for an Amazon Web
-- Services Region or location. Instance limits control the number of
-- instances, per instance type, per location, that your Amazon Web
-- Services account can use. Learn more at
-- <http://aws.amazon.com/ec2/instance-types/ Amazon EC2 Instance Types>.
-- The information returned includes the maximum number of instances
-- allowed and your account\'s current usage across all fleets. This
-- information can affect your ability to scale your GameLift fleets. You
-- can request a limit increase for your account by using the __Service
-- limits__ page in the GameLift console.
--
-- Instance limits differ based on whether the instances are deployed in a
-- fleet\'s home Region or in a remote location. For remote locations,
-- limits also differ based on the combination of home Region and remote
-- location. All requests must specify an Amazon Web Services Region
-- (either explicitly or as your default settings). To get the limit for a
-- remote location, you must also specify the location. For example, the
-- following requests all return different results:
--
-- -   Request specifies the Region @ap-northeast-1@ with no location. The
--     result is limits and usage data on all instance types that are
--     deployed in @us-east-2@, by all of the fleets that reside in
--     @ap-northeast-1@.
--
-- -   Request specifies the Region @us-east-1@ with location
--     @ca-central-1@. The result is limits and usage data on all instance
--     types that are deployed in @ca-central-1@, by all of the fleets that
--     reside in @us-east-2@. These limits do not affect fleets in any
--     other Regions that deploy instances to @ca-central-1@.
--
-- -   Request specifies the Region @eu-west-1@ with location
--     @ca-central-1@. The result is limits and usage data on all instance
--     types that are deployed in @ca-central-1@, by all of the fleets that
--     reside in @eu-west-1@.
--
-- This operation can be used in the following ways:
--
-- -   To get limit and usage data for all instance types that are deployed
--     in an Amazon Web Services Region by fleets that reside in the same
--     Region: Specify the Region only. Optionally, specify a single
--     instance type to retrieve information for.
--
-- -   To get limit and usage data for all instance types that are deployed
--     to a remote location by fleets that reside in different Amazon Web
--     Services Region: Provide both the Amazon Web Services Region and the
--     remote location. Optionally, specify a single instance type to
--     retrieve information for.
--
-- If successful, an @EC2InstanceLimits@ object is returned with limits and
-- usage data for each requested instance type.
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift fleets>
module Amazonka.GameLift.DescribeEC2InstanceLimits
  ( -- * Creating a Request
    DescribeEC2InstanceLimits (..),
    newDescribeEC2InstanceLimits,

    -- * Request Lenses
    describeEC2InstanceLimits_eC2InstanceType,
    describeEC2InstanceLimits_location,

    -- * Destructuring the Response
    DescribeEC2InstanceLimitsResponse (..),
    newDescribeEC2InstanceLimitsResponse,

    -- * Response Lenses
    describeEC2InstanceLimitsResponse_eC2InstanceLimits,
    describeEC2InstanceLimitsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEC2InstanceLimits' smart constructor.
data DescribeEC2InstanceLimits = DescribeEC2InstanceLimits'
  { -- | Name of an Amazon EC2 instance type that is supported in GameLift. A
    -- fleet instance type determines the computing resources of each instance
    -- in the fleet, including CPU, memory, storage, and networking capacity.
    -- Do not specify a value for this parameter to retrieve limits for all
    -- instance types.
    eC2InstanceType :: Prelude.Maybe EC2InstanceType,
    -- | The name of a remote location to request instance limits for, in the
    -- form of an Amazon Web Services Region code such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEC2InstanceLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2InstanceType', 'describeEC2InstanceLimits_eC2InstanceType' - Name of an Amazon EC2 instance type that is supported in GameLift. A
-- fleet instance type determines the computing resources of each instance
-- in the fleet, including CPU, memory, storage, and networking capacity.
-- Do not specify a value for this parameter to retrieve limits for all
-- instance types.
--
-- 'location', 'describeEC2InstanceLimits_location' - The name of a remote location to request instance limits for, in the
-- form of an Amazon Web Services Region code such as @us-west-2@.
newDescribeEC2InstanceLimits ::
  DescribeEC2InstanceLimits
newDescribeEC2InstanceLimits =
  DescribeEC2InstanceLimits'
    { eC2InstanceType =
        Prelude.Nothing,
      location = Prelude.Nothing
    }

-- | Name of an Amazon EC2 instance type that is supported in GameLift. A
-- fleet instance type determines the computing resources of each instance
-- in the fleet, including CPU, memory, storage, and networking capacity.
-- Do not specify a value for this parameter to retrieve limits for all
-- instance types.
describeEC2InstanceLimits_eC2InstanceType :: Lens.Lens' DescribeEC2InstanceLimits (Prelude.Maybe EC2InstanceType)
describeEC2InstanceLimits_eC2InstanceType = Lens.lens (\DescribeEC2InstanceLimits' {eC2InstanceType} -> eC2InstanceType) (\s@DescribeEC2InstanceLimits' {} a -> s {eC2InstanceType = a} :: DescribeEC2InstanceLimits)

-- | The name of a remote location to request instance limits for, in the
-- form of an Amazon Web Services Region code such as @us-west-2@.
describeEC2InstanceLimits_location :: Lens.Lens' DescribeEC2InstanceLimits (Prelude.Maybe Prelude.Text)
describeEC2InstanceLimits_location = Lens.lens (\DescribeEC2InstanceLimits' {location} -> location) (\s@DescribeEC2InstanceLimits' {} a -> s {location = a} :: DescribeEC2InstanceLimits)

instance Core.AWSRequest DescribeEC2InstanceLimits where
  type
    AWSResponse DescribeEC2InstanceLimits =
      DescribeEC2InstanceLimitsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEC2InstanceLimitsResponse'
            Prelude.<$> ( x
                            Data..?> "EC2InstanceLimits"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEC2InstanceLimits where
  hashWithSalt _salt DescribeEC2InstanceLimits' {..} =
    _salt
      `Prelude.hashWithSalt` eC2InstanceType
      `Prelude.hashWithSalt` location

instance Prelude.NFData DescribeEC2InstanceLimits where
  rnf DescribeEC2InstanceLimits' {..} =
    Prelude.rnf eC2InstanceType
      `Prelude.seq` Prelude.rnf location

instance Data.ToHeaders DescribeEC2InstanceLimits where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeEC2InstanceLimits" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEC2InstanceLimits where
  toJSON DescribeEC2InstanceLimits' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EC2InstanceType" Data..=)
              Prelude.<$> eC2InstanceType,
            ("Location" Data..=) Prelude.<$> location
          ]
      )

instance Data.ToPath DescribeEC2InstanceLimits where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEC2InstanceLimits where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEC2InstanceLimitsResponse' smart constructor.
data DescribeEC2InstanceLimitsResponse = DescribeEC2InstanceLimitsResponse'
  { -- | The maximum number of instances for the specified instance type.
    eC2InstanceLimits :: Prelude.Maybe [EC2InstanceLimit],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEC2InstanceLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eC2InstanceLimits', 'describeEC2InstanceLimitsResponse_eC2InstanceLimits' - The maximum number of instances for the specified instance type.
--
-- 'httpStatus', 'describeEC2InstanceLimitsResponse_httpStatus' - The response's http status code.
newDescribeEC2InstanceLimitsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEC2InstanceLimitsResponse
newDescribeEC2InstanceLimitsResponse pHttpStatus_ =
  DescribeEC2InstanceLimitsResponse'
    { eC2InstanceLimits =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The maximum number of instances for the specified instance type.
describeEC2InstanceLimitsResponse_eC2InstanceLimits :: Lens.Lens' DescribeEC2InstanceLimitsResponse (Prelude.Maybe [EC2InstanceLimit])
describeEC2InstanceLimitsResponse_eC2InstanceLimits = Lens.lens (\DescribeEC2InstanceLimitsResponse' {eC2InstanceLimits} -> eC2InstanceLimits) (\s@DescribeEC2InstanceLimitsResponse' {} a -> s {eC2InstanceLimits = a} :: DescribeEC2InstanceLimitsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEC2InstanceLimitsResponse_httpStatus :: Lens.Lens' DescribeEC2InstanceLimitsResponse Prelude.Int
describeEC2InstanceLimitsResponse_httpStatus = Lens.lens (\DescribeEC2InstanceLimitsResponse' {httpStatus} -> httpStatus) (\s@DescribeEC2InstanceLimitsResponse' {} a -> s {httpStatus = a} :: DescribeEC2InstanceLimitsResponse)

instance
  Prelude.NFData
    DescribeEC2InstanceLimitsResponse
  where
  rnf DescribeEC2InstanceLimitsResponse' {..} =
    Prelude.rnf eC2InstanceLimits
      `Prelude.seq` Prelude.rnf httpStatus
