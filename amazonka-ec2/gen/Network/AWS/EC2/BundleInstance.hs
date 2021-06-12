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
-- Module      : Network.AWS.EC2.BundleInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Bundles an Amazon instance store-backed Windows instance.
--
-- During bundling, only the root device volume (C:\\) is bundled. Data on
-- other instance store volumes is not preserved.
--
-- This action is not applicable for Linux\/Unix instances or Windows
-- instances that are backed by Amazon EBS.
module Network.AWS.EC2.BundleInstance
  ( -- * Creating a Request
    BundleInstance (..),
    newBundleInstance,

    -- * Request Lenses
    bundleInstance_dryRun,
    bundleInstance_instanceId,
    bundleInstance_storage,

    -- * Destructuring the Response
    BundleInstanceResponse (..),
    newBundleInstanceResponse,

    -- * Response Lenses
    bundleInstanceResponse_bundleTask,
    bundleInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for BundleInstance.
--
-- /See:/ 'newBundleInstance' smart constructor.
data BundleInstance = BundleInstance'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the instance to bundle.
    --
    -- Type: String
    --
    -- Default: None
    --
    -- Required: Yes
    instanceId :: Core.Text,
    -- | The bucket in which to store the AMI. You can specify a bucket that you
    -- already own or a new bucket that Amazon EC2 creates on your behalf. If
    -- you specify a bucket that belongs to someone else, Amazon EC2 returns an
    -- error.
    storage :: Storage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BundleInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'bundleInstance_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'bundleInstance_instanceId' - The ID of the instance to bundle.
--
-- Type: String
--
-- Default: None
--
-- Required: Yes
--
-- 'storage', 'bundleInstance_storage' - The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If
-- you specify a bucket that belongs to someone else, Amazon EC2 returns an
-- error.
newBundleInstance ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'storage'
  Storage ->
  BundleInstance
newBundleInstance pInstanceId_ pStorage_ =
  BundleInstance'
    { dryRun = Core.Nothing,
      instanceId = pInstanceId_,
      storage = pStorage_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
bundleInstance_dryRun :: Lens.Lens' BundleInstance (Core.Maybe Core.Bool)
bundleInstance_dryRun = Lens.lens (\BundleInstance' {dryRun} -> dryRun) (\s@BundleInstance' {} a -> s {dryRun = a} :: BundleInstance)

-- | The ID of the instance to bundle.
--
-- Type: String
--
-- Default: None
--
-- Required: Yes
bundleInstance_instanceId :: Lens.Lens' BundleInstance Core.Text
bundleInstance_instanceId = Lens.lens (\BundleInstance' {instanceId} -> instanceId) (\s@BundleInstance' {} a -> s {instanceId = a} :: BundleInstance)

-- | The bucket in which to store the AMI. You can specify a bucket that you
-- already own or a new bucket that Amazon EC2 creates on your behalf. If
-- you specify a bucket that belongs to someone else, Amazon EC2 returns an
-- error.
bundleInstance_storage :: Lens.Lens' BundleInstance Storage
bundleInstance_storage = Lens.lens (\BundleInstance' {storage} -> storage) (\s@BundleInstance' {} a -> s {storage = a} :: BundleInstance)

instance Core.AWSRequest BundleInstance where
  type
    AWSResponse BundleInstance =
      BundleInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          BundleInstanceResponse'
            Core.<$> (x Core..@? "bundleInstanceTask")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BundleInstance

instance Core.NFData BundleInstance

instance Core.ToHeaders BundleInstance where
  toHeaders = Core.const Core.mempty

instance Core.ToPath BundleInstance where
  toPath = Core.const "/"

instance Core.ToQuery BundleInstance where
  toQuery BundleInstance' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("BundleInstance" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "InstanceId" Core.=: instanceId,
        "Storage" Core.=: storage
      ]

-- | Contains the output of BundleInstance.
--
-- /See:/ 'newBundleInstanceResponse' smart constructor.
data BundleInstanceResponse = BundleInstanceResponse'
  { -- | Information about the bundle task.
    bundleTask :: Core.Maybe BundleTask,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BundleInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundleTask', 'bundleInstanceResponse_bundleTask' - Information about the bundle task.
--
-- 'httpStatus', 'bundleInstanceResponse_httpStatus' - The response's http status code.
newBundleInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BundleInstanceResponse
newBundleInstanceResponse pHttpStatus_ =
  BundleInstanceResponse'
    { bundleTask = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the bundle task.
bundleInstanceResponse_bundleTask :: Lens.Lens' BundleInstanceResponse (Core.Maybe BundleTask)
bundleInstanceResponse_bundleTask = Lens.lens (\BundleInstanceResponse' {bundleTask} -> bundleTask) (\s@BundleInstanceResponse' {} a -> s {bundleTask = a} :: BundleInstanceResponse)

-- | The response's http status code.
bundleInstanceResponse_httpStatus :: Lens.Lens' BundleInstanceResponse Core.Int
bundleInstanceResponse_httpStatus = Lens.lens (\BundleInstanceResponse' {httpStatus} -> httpStatus) (\s@BundleInstanceResponse' {} a -> s {httpStatus = a} :: BundleInstanceResponse)

instance Core.NFData BundleInstanceResponse
