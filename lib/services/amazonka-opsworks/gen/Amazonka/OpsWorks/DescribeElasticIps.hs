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
-- Module      : Amazonka.OpsWorks.DescribeElasticIps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/elastic-ip-addresses-eip.html Elastic IP addresses>.
--
-- This call accepts only one resource-identifying parameter.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Amazonka.OpsWorks.DescribeElasticIps
  ( -- * Creating a Request
    DescribeElasticIps (..),
    newDescribeElasticIps,

    -- * Request Lenses
    describeElasticIps_instanceId,
    describeElasticIps_ips,
    describeElasticIps_stackId,

    -- * Destructuring the Response
    DescribeElasticIpsResponse (..),
    newDescribeElasticIpsResponse,

    -- * Response Lenses
    describeElasticIpsResponse_elasticIps,
    describeElasticIpsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeElasticIps' smart constructor.
data DescribeElasticIps = DescribeElasticIps'
  { -- | The instance ID. If you include this parameter, @DescribeElasticIps@
    -- returns a description of the Elastic IP addresses associated with the
    -- specified instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | An array of Elastic IP addresses to be described. If you include this
    -- parameter, @DescribeElasticIps@ returns a description of the specified
    -- Elastic IP addresses. Otherwise, it returns a description of every
    -- Elastic IP address.
    ips :: Prelude.Maybe [Prelude.Text],
    -- | A stack ID. If you include this parameter, @DescribeElasticIps@ returns
    -- a description of the Elastic IP addresses that are registered with the
    -- specified stack.
    stackId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticIps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeElasticIps_instanceId' - The instance ID. If you include this parameter, @DescribeElasticIps@
-- returns a description of the Elastic IP addresses associated with the
-- specified instance.
--
-- 'ips', 'describeElasticIps_ips' - An array of Elastic IP addresses to be described. If you include this
-- parameter, @DescribeElasticIps@ returns a description of the specified
-- Elastic IP addresses. Otherwise, it returns a description of every
-- Elastic IP address.
--
-- 'stackId', 'describeElasticIps_stackId' - A stack ID. If you include this parameter, @DescribeElasticIps@ returns
-- a description of the Elastic IP addresses that are registered with the
-- specified stack.
newDescribeElasticIps ::
  DescribeElasticIps
newDescribeElasticIps =
  DescribeElasticIps'
    { instanceId = Prelude.Nothing,
      ips = Prelude.Nothing,
      stackId = Prelude.Nothing
    }

-- | The instance ID. If you include this parameter, @DescribeElasticIps@
-- returns a description of the Elastic IP addresses associated with the
-- specified instance.
describeElasticIps_instanceId :: Lens.Lens' DescribeElasticIps (Prelude.Maybe Prelude.Text)
describeElasticIps_instanceId = Lens.lens (\DescribeElasticIps' {instanceId} -> instanceId) (\s@DescribeElasticIps' {} a -> s {instanceId = a} :: DescribeElasticIps)

-- | An array of Elastic IP addresses to be described. If you include this
-- parameter, @DescribeElasticIps@ returns a description of the specified
-- Elastic IP addresses. Otherwise, it returns a description of every
-- Elastic IP address.
describeElasticIps_ips :: Lens.Lens' DescribeElasticIps (Prelude.Maybe [Prelude.Text])
describeElasticIps_ips = Lens.lens (\DescribeElasticIps' {ips} -> ips) (\s@DescribeElasticIps' {} a -> s {ips = a} :: DescribeElasticIps) Prelude.. Lens.mapping Lens.coerced

-- | A stack ID. If you include this parameter, @DescribeElasticIps@ returns
-- a description of the Elastic IP addresses that are registered with the
-- specified stack.
describeElasticIps_stackId :: Lens.Lens' DescribeElasticIps (Prelude.Maybe Prelude.Text)
describeElasticIps_stackId = Lens.lens (\DescribeElasticIps' {stackId} -> stackId) (\s@DescribeElasticIps' {} a -> s {stackId = a} :: DescribeElasticIps)

instance Core.AWSRequest DescribeElasticIps where
  type
    AWSResponse DescribeElasticIps =
      DescribeElasticIpsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticIpsResponse'
            Prelude.<$> (x Data..?> "ElasticIps" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeElasticIps where
  hashWithSalt _salt DescribeElasticIps' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` ips
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData DescribeElasticIps where
  rnf DescribeElasticIps' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf ips
      `Prelude.seq` Prelude.rnf stackId

instance Data.ToHeaders DescribeElasticIps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeElasticIps" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeElasticIps where
  toJSON DescribeElasticIps' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("Ips" Data..=) Prelude.<$> ips,
            ("StackId" Data..=) Prelude.<$> stackId
          ]
      )

instance Data.ToPath DescribeElasticIps where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeElasticIps where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeElasticIps@ request.
--
-- /See:/ 'newDescribeElasticIpsResponse' smart constructor.
data DescribeElasticIpsResponse = DescribeElasticIpsResponse'
  { -- | An @ElasticIps@ object that describes the specified Elastic IP
    -- addresses.
    elasticIps :: Prelude.Maybe [ElasticIp],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticIpsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticIps', 'describeElasticIpsResponse_elasticIps' - An @ElasticIps@ object that describes the specified Elastic IP
-- addresses.
--
-- 'httpStatus', 'describeElasticIpsResponse_httpStatus' - The response's http status code.
newDescribeElasticIpsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeElasticIpsResponse
newDescribeElasticIpsResponse pHttpStatus_ =
  DescribeElasticIpsResponse'
    { elasticIps =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An @ElasticIps@ object that describes the specified Elastic IP
-- addresses.
describeElasticIpsResponse_elasticIps :: Lens.Lens' DescribeElasticIpsResponse (Prelude.Maybe [ElasticIp])
describeElasticIpsResponse_elasticIps = Lens.lens (\DescribeElasticIpsResponse' {elasticIps} -> elasticIps) (\s@DescribeElasticIpsResponse' {} a -> s {elasticIps = a} :: DescribeElasticIpsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeElasticIpsResponse_httpStatus :: Lens.Lens' DescribeElasticIpsResponse Prelude.Int
describeElasticIpsResponse_httpStatus = Lens.lens (\DescribeElasticIpsResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticIpsResponse' {} a -> s {httpStatus = a} :: DescribeElasticIpsResponse)

instance Prelude.NFData DescribeElasticIpsResponse where
  rnf DescribeElasticIpsResponse' {..} =
    Prelude.rnf elasticIps
      `Prelude.seq` Prelude.rnf httpStatus
