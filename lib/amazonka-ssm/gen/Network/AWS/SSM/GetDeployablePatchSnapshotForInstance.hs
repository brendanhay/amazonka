{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetDeployablePatchSnapshotForInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current snapshot for the patch baseline the instance uses. This API is primarily used by the AWS-RunPatchBaseline Systems Manager document.
module Network.AWS.SSM.GetDeployablePatchSnapshotForInstance
  ( -- * Creating a request
    GetDeployablePatchSnapshotForInstance (..),
    mkGetDeployablePatchSnapshotForInstance,

    -- ** Request lenses
    gdpsfiInstanceId,
    gdpsfiSnapshotId,

    -- * Destructuring the response
    GetDeployablePatchSnapshotForInstanceResponse (..),
    mkGetDeployablePatchSnapshotForInstanceResponse,

    -- ** Response lenses
    gdpsfirsInstanceId,
    gdpsfirsProduct,
    gdpsfirsSnapshotDownloadURL,
    gdpsfirsSnapshotId,
    gdpsfirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkGetDeployablePatchSnapshotForInstance' smart constructor.
data GetDeployablePatchSnapshotForInstance = GetDeployablePatchSnapshotForInstance'
  { instanceId ::
      Lude.Text,
    snapshotId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDeployablePatchSnapshotForInstance' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance for which the appropriate patch snapshot should be retrieved.
-- * 'snapshotId' - The user-defined snapshot ID.
mkGetDeployablePatchSnapshotForInstance ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'snapshotId'
  Lude.Text ->
  GetDeployablePatchSnapshotForInstance
mkGetDeployablePatchSnapshotForInstance pInstanceId_ pSnapshotId_ =
  GetDeployablePatchSnapshotForInstance'
    { instanceId = pInstanceId_,
      snapshotId = pSnapshotId_
    }

-- | The ID of the instance for which the appropriate patch snapshot should be retrieved.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfiInstanceId :: Lens.Lens' GetDeployablePatchSnapshotForInstance Lude.Text
gdpsfiInstanceId = Lens.lens (instanceId :: GetDeployablePatchSnapshotForInstance -> Lude.Text) (\s a -> s {instanceId = a} :: GetDeployablePatchSnapshotForInstance)
{-# DEPRECATED gdpsfiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The user-defined snapshot ID.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfiSnapshotId :: Lens.Lens' GetDeployablePatchSnapshotForInstance Lude.Text
gdpsfiSnapshotId = Lens.lens (snapshotId :: GetDeployablePatchSnapshotForInstance -> Lude.Text) (\s a -> s {snapshotId = a} :: GetDeployablePatchSnapshotForInstance)
{-# DEPRECATED gdpsfiSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.AWSRequest GetDeployablePatchSnapshotForInstance where
  type
    Rs GetDeployablePatchSnapshotForInstance =
      GetDeployablePatchSnapshotForInstanceResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDeployablePatchSnapshotForInstanceResponse'
            Lude.<$> (x Lude..?> "InstanceId")
            Lude.<*> (x Lude..?> "Product")
            Lude.<*> (x Lude..?> "SnapshotDownloadUrl")
            Lude.<*> (x Lude..?> "SnapshotId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDeployablePatchSnapshotForInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.GetDeployablePatchSnapshotForInstance" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDeployablePatchSnapshotForInstance where
  toJSON GetDeployablePatchSnapshotForInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("SnapshotId" Lude..= snapshotId)
          ]
      )

instance Lude.ToPath GetDeployablePatchSnapshotForInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDeployablePatchSnapshotForInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDeployablePatchSnapshotForInstanceResponse' smart constructor.
data GetDeployablePatchSnapshotForInstanceResponse = GetDeployablePatchSnapshotForInstanceResponse'
  { instanceId ::
      Lude.Maybe
        Lude.Text,
    product ::
      Lude.Maybe
        Lude.Text,
    snapshotDownloadURL ::
      Lude.Maybe
        Lude.Text,
    snapshotId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'GetDeployablePatchSnapshotForInstanceResponse' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'product' - Returns the specific operating system (for example Windows Server 2012 or Amazon Linux 2015.09) on the instance for the specified patch snapshot.
-- * 'responseStatus' - The response status code.
-- * 'snapshotDownloadURL' - A pre-signed Amazon S3 URL that can be used to download the patch snapshot.
-- * 'snapshotId' - The user-defined snapshot ID.
mkGetDeployablePatchSnapshotForInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDeployablePatchSnapshotForInstanceResponse
mkGetDeployablePatchSnapshotForInstanceResponse pResponseStatus_ =
  GetDeployablePatchSnapshotForInstanceResponse'
    { instanceId =
        Lude.Nothing,
      product = Lude.Nothing,
      snapshotDownloadURL = Lude.Nothing,
      snapshotId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfirsInstanceId :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Lude.Maybe Lude.Text)
gdpsfirsInstanceId = Lens.lens (instanceId :: GetDeployablePatchSnapshotForInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: GetDeployablePatchSnapshotForInstanceResponse)
{-# DEPRECATED gdpsfirsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Returns the specific operating system (for example Windows Server 2012 or Amazon Linux 2015.09) on the instance for the specified patch snapshot.
--
-- /Note:/ Consider using 'product' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfirsProduct :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Lude.Maybe Lude.Text)
gdpsfirsProduct = Lens.lens (product :: GetDeployablePatchSnapshotForInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {product = a} :: GetDeployablePatchSnapshotForInstanceResponse)
{-# DEPRECATED gdpsfirsProduct "Use generic-lens or generic-optics with 'product' instead." #-}

-- | A pre-signed Amazon S3 URL that can be used to download the patch snapshot.
--
-- /Note:/ Consider using 'snapshotDownloadURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfirsSnapshotDownloadURL :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Lude.Maybe Lude.Text)
gdpsfirsSnapshotDownloadURL = Lens.lens (snapshotDownloadURL :: GetDeployablePatchSnapshotForInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {snapshotDownloadURL = a} :: GetDeployablePatchSnapshotForInstanceResponse)
{-# DEPRECATED gdpsfirsSnapshotDownloadURL "Use generic-lens or generic-optics with 'snapshotDownloadURL' instead." #-}

-- | The user-defined snapshot ID.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfirsSnapshotId :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Lude.Maybe Lude.Text)
gdpsfirsSnapshotId = Lens.lens (snapshotId :: GetDeployablePatchSnapshotForInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: GetDeployablePatchSnapshotForInstanceResponse)
{-# DEPRECATED gdpsfirsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpsfirsResponseStatus :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse Lude.Int
gdpsfirsResponseStatus = Lens.lens (responseStatus :: GetDeployablePatchSnapshotForInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDeployablePatchSnapshotForInstanceResponse)
{-# DEPRECATED gdpsfirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
