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
-- Module      : Network.AWS.EC2.DeregisterImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified AMI. After you deregister an AMI, it can\'t be
-- used to launch new instances; however, it doesn\'t affect any instances
-- that you\'ve already launched from the AMI. You\'ll continue to incur
-- usage costs for those instances until you terminate them.
--
-- When you deregister an Amazon EBS-backed AMI, it doesn\'t affect the
-- snapshot that was created for the root volume of the instance during the
-- AMI creation process. When you deregister an instance store-backed AMI,
-- it doesn\'t affect the files that you uploaded to Amazon S3 when you
-- created the AMI.
module Network.AWS.EC2.DeregisterImage
  ( -- * Creating a Request
    DeregisterImage (..),
    newDeregisterImage,

    -- * Request Lenses
    deregisterImage_dryRun,
    deregisterImage_imageId,

    -- * Destructuring the Response
    DeregisterImageResponse (..),
    newDeregisterImageResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeregisterImage.
--
-- /See:/ 'newDeregisterImage' smart constructor.
data DeregisterImage = DeregisterImage'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the AMI.
    imageId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deregisterImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'imageId', 'deregisterImage_imageId' - The ID of the AMI.
newDeregisterImage ::
  -- | 'imageId'
  Core.Text ->
  DeregisterImage
newDeregisterImage pImageId_ =
  DeregisterImage'
    { dryRun = Core.Nothing,
      imageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deregisterImage_dryRun :: Lens.Lens' DeregisterImage (Core.Maybe Core.Bool)
deregisterImage_dryRun = Lens.lens (\DeregisterImage' {dryRun} -> dryRun) (\s@DeregisterImage' {} a -> s {dryRun = a} :: DeregisterImage)

-- | The ID of the AMI.
deregisterImage_imageId :: Lens.Lens' DeregisterImage Core.Text
deregisterImage_imageId = Lens.lens (\DeregisterImage' {imageId} -> imageId) (\s@DeregisterImage' {} a -> s {imageId = a} :: DeregisterImage)

instance Core.AWSRequest DeregisterImage where
  type
    AWSResponse DeregisterImage =
      DeregisterImageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeregisterImageResponse'

instance Core.Hashable DeregisterImage

instance Core.NFData DeregisterImage

instance Core.ToHeaders DeregisterImage where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeregisterImage where
  toPath = Core.const "/"

instance Core.ToQuery DeregisterImage where
  toQuery DeregisterImage' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeregisterImage" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "ImageId" Core.=: imageId
      ]

-- | /See:/ 'newDeregisterImageResponse' smart constructor.
data DeregisterImageResponse = DeregisterImageResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeregisterImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterImageResponse ::
  DeregisterImageResponse
newDeregisterImageResponse = DeregisterImageResponse'

instance Core.NFData DeregisterImageResponse
