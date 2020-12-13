{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.GetLaunchTemplateData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configuration data of the specified instance. You can use this data to create a launch template.
--
-- This action calls on other describe actions to get instance information. Depending on your instance configuration, you may need to allow the following actions in your IAM policy: DescribeSpotInstanceRequests, DescribeInstanceCreditSpecifications, DescribeVolumes, DescribeInstanceAttribute, and DescribeElasticGpus. Or, you can allow @describe*@ depending on your instance requirements.
module Network.AWS.EC2.GetLaunchTemplateData
  ( -- * Creating a request
    GetLaunchTemplateData (..),
    mkGetLaunchTemplateData,

    -- ** Request lenses
    gltdInstanceId,
    gltdDryRun,

    -- * Destructuring the response
    GetLaunchTemplateDataResponse (..),
    mkGetLaunchTemplateDataResponse,

    -- ** Response lenses
    gltdrsLaunchTemplateData,
    gltdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLaunchTemplateData' smart constructor.
data GetLaunchTemplateData = GetLaunchTemplateData'
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLaunchTemplateData' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkGetLaunchTemplateData ::
  -- | 'instanceId'
  Lude.Text ->
  GetLaunchTemplateData
mkGetLaunchTemplateData pInstanceId_ =
  GetLaunchTemplateData'
    { instanceId = pInstanceId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdInstanceId :: Lens.Lens' GetLaunchTemplateData Lude.Text
gltdInstanceId = Lens.lens (instanceId :: GetLaunchTemplateData -> Lude.Text) (\s a -> s {instanceId = a} :: GetLaunchTemplateData)
{-# DEPRECATED gltdInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdDryRun :: Lens.Lens' GetLaunchTemplateData (Lude.Maybe Lude.Bool)
gltdDryRun = Lens.lens (dryRun :: GetLaunchTemplateData -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: GetLaunchTemplateData)
{-# DEPRECATED gltdDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest GetLaunchTemplateData where
  type Rs GetLaunchTemplateData = GetLaunchTemplateDataResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetLaunchTemplateDataResponse'
            Lude.<$> (x Lude..@? "launchTemplateData")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLaunchTemplateData where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetLaunchTemplateData where
  toPath = Lude.const "/"

instance Lude.ToQuery GetLaunchTemplateData where
  toQuery GetLaunchTemplateData' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetLaunchTemplateData" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkGetLaunchTemplateDataResponse' smart constructor.
data GetLaunchTemplateDataResponse = GetLaunchTemplateDataResponse'
  { -- | The instance data.
    launchTemplateData :: Lude.Maybe ResponseLaunchTemplateData,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLaunchTemplateDataResponse' with the minimum fields required to make a request.
--
-- * 'launchTemplateData' - The instance data.
-- * 'responseStatus' - The response status code.
mkGetLaunchTemplateDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLaunchTemplateDataResponse
mkGetLaunchTemplateDataResponse pResponseStatus_ =
  GetLaunchTemplateDataResponse'
    { launchTemplateData = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The instance data.
--
-- /Note:/ Consider using 'launchTemplateData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdrsLaunchTemplateData :: Lens.Lens' GetLaunchTemplateDataResponse (Lude.Maybe ResponseLaunchTemplateData)
gltdrsLaunchTemplateData = Lens.lens (launchTemplateData :: GetLaunchTemplateDataResponse -> Lude.Maybe ResponseLaunchTemplateData) (\s a -> s {launchTemplateData = a} :: GetLaunchTemplateDataResponse)
{-# DEPRECATED gltdrsLaunchTemplateData "Use generic-lens or generic-optics with 'launchTemplateData' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gltdrsResponseStatus :: Lens.Lens' GetLaunchTemplateDataResponse Lude.Int
gltdrsResponseStatus = Lens.lens (responseStatus :: GetLaunchTemplateDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLaunchTemplateDataResponse)
{-# DEPRECATED gltdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
