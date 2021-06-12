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
-- Module      : Network.AWS.OpsWorks.DescribeElasticIps
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.OpsWorks.DescribeElasticIps
  ( -- * Creating a Request
    DescribeElasticIps (..),
    newDescribeElasticIps,

    -- * Request Lenses
    describeElasticIps_instanceId,
    describeElasticIps_stackId,
    describeElasticIps_ips,

    -- * Destructuring the Response
    DescribeElasticIpsResponse (..),
    newDescribeElasticIpsResponse,

    -- * Response Lenses
    describeElasticIpsResponse_elasticIps,
    describeElasticIpsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeElasticIps' smart constructor.
data DescribeElasticIps = DescribeElasticIps'
  { -- | The instance ID. If you include this parameter, @DescribeElasticIps@
    -- returns a description of the Elastic IP addresses associated with the
    -- specified instance.
    instanceId :: Core.Maybe Core.Text,
    -- | A stack ID. If you include this parameter, @DescribeElasticIps@ returns
    -- a description of the Elastic IP addresses that are registered with the
    -- specified stack.
    stackId :: Core.Maybe Core.Text,
    -- | An array of Elastic IP addresses to be described. If you include this
    -- parameter, @DescribeElasticIps@ returns a description of the specified
    -- Elastic IP addresses. Otherwise, it returns a description of every
    -- Elastic IP address.
    ips :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'stackId', 'describeElasticIps_stackId' - A stack ID. If you include this parameter, @DescribeElasticIps@ returns
-- a description of the Elastic IP addresses that are registered with the
-- specified stack.
--
-- 'ips', 'describeElasticIps_ips' - An array of Elastic IP addresses to be described. If you include this
-- parameter, @DescribeElasticIps@ returns a description of the specified
-- Elastic IP addresses. Otherwise, it returns a description of every
-- Elastic IP address.
newDescribeElasticIps ::
  DescribeElasticIps
newDescribeElasticIps =
  DescribeElasticIps'
    { instanceId = Core.Nothing,
      stackId = Core.Nothing,
      ips = Core.Nothing
    }

-- | The instance ID. If you include this parameter, @DescribeElasticIps@
-- returns a description of the Elastic IP addresses associated with the
-- specified instance.
describeElasticIps_instanceId :: Lens.Lens' DescribeElasticIps (Core.Maybe Core.Text)
describeElasticIps_instanceId = Lens.lens (\DescribeElasticIps' {instanceId} -> instanceId) (\s@DescribeElasticIps' {} a -> s {instanceId = a} :: DescribeElasticIps)

-- | A stack ID. If you include this parameter, @DescribeElasticIps@ returns
-- a description of the Elastic IP addresses that are registered with the
-- specified stack.
describeElasticIps_stackId :: Lens.Lens' DescribeElasticIps (Core.Maybe Core.Text)
describeElasticIps_stackId = Lens.lens (\DescribeElasticIps' {stackId} -> stackId) (\s@DescribeElasticIps' {} a -> s {stackId = a} :: DescribeElasticIps)

-- | An array of Elastic IP addresses to be described. If you include this
-- parameter, @DescribeElasticIps@ returns a description of the specified
-- Elastic IP addresses. Otherwise, it returns a description of every
-- Elastic IP address.
describeElasticIps_ips :: Lens.Lens' DescribeElasticIps (Core.Maybe [Core.Text])
describeElasticIps_ips = Lens.lens (\DescribeElasticIps' {ips} -> ips) (\s@DescribeElasticIps' {} a -> s {ips = a} :: DescribeElasticIps) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeElasticIps where
  type
    AWSResponse DescribeElasticIps =
      DescribeElasticIpsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticIpsResponse'
            Core.<$> (x Core..?> "ElasticIps" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeElasticIps

instance Core.NFData DescribeElasticIps

instance Core.ToHeaders DescribeElasticIps where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeElasticIps" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeElasticIps where
  toJSON DescribeElasticIps' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceId" Core..=) Core.<$> instanceId,
            ("StackId" Core..=) Core.<$> stackId,
            ("Ips" Core..=) Core.<$> ips
          ]
      )

instance Core.ToPath DescribeElasticIps where
  toPath = Core.const "/"

instance Core.ToQuery DescribeElasticIps where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeElasticIps@ request.
--
-- /See:/ 'newDescribeElasticIpsResponse' smart constructor.
data DescribeElasticIpsResponse = DescribeElasticIpsResponse'
  { -- | An @ElasticIps@ object that describes the specified Elastic IP
    -- addresses.
    elasticIps :: Core.Maybe [ElasticIp],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeElasticIpsResponse
newDescribeElasticIpsResponse pHttpStatus_ =
  DescribeElasticIpsResponse'
    { elasticIps =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An @ElasticIps@ object that describes the specified Elastic IP
-- addresses.
describeElasticIpsResponse_elasticIps :: Lens.Lens' DescribeElasticIpsResponse (Core.Maybe [ElasticIp])
describeElasticIpsResponse_elasticIps = Lens.lens (\DescribeElasticIpsResponse' {elasticIps} -> elasticIps) (\s@DescribeElasticIpsResponse' {} a -> s {elasticIps = a} :: DescribeElasticIpsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeElasticIpsResponse_httpStatus :: Lens.Lens' DescribeElasticIpsResponse Core.Int
describeElasticIpsResponse_httpStatus = Lens.lens (\DescribeElasticIpsResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticIpsResponse' {} a -> s {httpStatus = a} :: DescribeElasticIpsResponse)

instance Core.NFData DescribeElasticIpsResponse
