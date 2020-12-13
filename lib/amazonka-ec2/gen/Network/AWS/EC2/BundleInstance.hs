{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.BundleInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Bundles an Amazon instance store-backed Windows instance.
--
-- During bundling, only the root device volume (C:\) is bundled. Data on other instance store volumes is not preserved.
module Network.AWS.EC2.BundleInstance
  ( -- * Creating a request
    BundleInstance (..),
    mkBundleInstance,

    -- ** Request lenses
    biInstanceId,
    biStorage,
    biDryRun,

    -- * Destructuring the response
    BundleInstanceResponse (..),
    mkBundleInstanceResponse,

    -- ** Response lenses
    birsBundleTask,
    birsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for BundleInstance.
--
-- /See:/ 'mkBundleInstance' smart constructor.
data BundleInstance = BundleInstance'
  { -- | The ID of the instance to bundle.
    --
    -- Type: String
    -- Default: None
    -- Required: Yes
    instanceId :: Lude.Text,
    -- | The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
    storage :: Storage,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BundleInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance to bundle.
--
-- Type: String
-- Default: None
-- Required: Yes
-- * 'storage' - The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkBundleInstance ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'storage'
  Storage ->
  BundleInstance
mkBundleInstance pInstanceId_ pStorage_ =
  BundleInstance'
    { instanceId = pInstanceId_,
      storage = pStorage_,
      dryRun = Lude.Nothing
    }

-- | The ID of the instance to bundle.
--
-- Type: String
-- Default: None
-- Required: Yes
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biInstanceId :: Lens.Lens' BundleInstance Lude.Text
biInstanceId = Lens.lens (instanceId :: BundleInstance -> Lude.Text) (\s a -> s {instanceId = a} :: BundleInstance)
{-# DEPRECATED biInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The bucket in which to store the AMI. You can specify a bucket that you already own or a new bucket that Amazon EC2 creates on your behalf. If you specify a bucket that belongs to someone else, Amazon EC2 returns an error.
--
-- /Note:/ Consider using 'storage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biStorage :: Lens.Lens' BundleInstance Storage
biStorage = Lens.lens (storage :: BundleInstance -> Storage) (\s a -> s {storage = a} :: BundleInstance)
{-# DEPRECATED biStorage "Use generic-lens or generic-optics with 'storage' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
biDryRun :: Lens.Lens' BundleInstance (Lude.Maybe Lude.Bool)
biDryRun = Lens.lens (dryRun :: BundleInstance -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: BundleInstance)
{-# DEPRECATED biDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest BundleInstance where
  type Rs BundleInstance = BundleInstanceResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          BundleInstanceResponse'
            Lude.<$> (x Lude..@? "bundleInstanceTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BundleInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BundleInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery BundleInstance where
  toQuery BundleInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("BundleInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "InstanceId" Lude.=: instanceId,
        "Storage" Lude.=: storage,
        "DryRun" Lude.=: dryRun
      ]

-- | Contains the output of BundleInstance.
--
-- /See:/ 'mkBundleInstanceResponse' smart constructor.
data BundleInstanceResponse = BundleInstanceResponse'
  { -- | Information about the bundle task.
    bundleTask :: Lude.Maybe BundleTask,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BundleInstanceResponse' with the minimum fields required to make a request.
--
-- * 'bundleTask' - Information about the bundle task.
-- * 'responseStatus' - The response status code.
mkBundleInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BundleInstanceResponse
mkBundleInstanceResponse pResponseStatus_ =
  BundleInstanceResponse'
    { bundleTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the bundle task.
--
-- /Note:/ Consider using 'bundleTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
birsBundleTask :: Lens.Lens' BundleInstanceResponse (Lude.Maybe BundleTask)
birsBundleTask = Lens.lens (bundleTask :: BundleInstanceResponse -> Lude.Maybe BundleTask) (\s a -> s {bundleTask = a} :: BundleInstanceResponse)
{-# DEPRECATED birsBundleTask "Use generic-lens or generic-optics with 'bundleTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
birsResponseStatus :: Lens.Lens' BundleInstanceResponse Lude.Int
birsResponseStatus = Lens.lens (responseStatus :: BundleInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BundleInstanceResponse)
{-# DEPRECATED birsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
