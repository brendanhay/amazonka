{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the gateway volumes specified in the request. This operation is only supported in the cached volume gateway types.
--
-- The list of gateway volumes in the request must be from one gateway. In the response, AWS Storage Gateway returns volume information sorted by volume Amazon Resource Name (ARN).
module Network.AWS.StorageGateway.DescribeCachediSCSIVolumes
  ( -- * Creating a request
    DescribeCachediSCSIVolumes (..),
    mkDescribeCachediSCSIVolumes,

    -- ** Request lenses
    dcscsivVolumeARNs,

    -- * Destructuring the response
    DescribeCachediSCSIVolumesResponse (..),
    mkDescribeCachediSCSIVolumesResponse,

    -- ** Response lenses
    dcscsivrsCachediSCSIVolumes,
    dcscsivrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkDescribeCachediSCSIVolumes' smart constructor.
newtype DescribeCachediSCSIVolumes = DescribeCachediSCSIVolumes'
  { volumeARNs ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCachediSCSIVolumes' with the minimum fields required to make a request.
--
-- * 'volumeARNs' - An array of strings where each string represents the Amazon Resource Name (ARN) of a cached volume. All of the specified cached volumes must be from the same gateway. Use 'ListVolumes' to get volume ARNs for a gateway.
mkDescribeCachediSCSIVolumes ::
  DescribeCachediSCSIVolumes
mkDescribeCachediSCSIVolumes =
  DescribeCachediSCSIVolumes' {volumeARNs = Lude.mempty}

-- | An array of strings where each string represents the Amazon Resource Name (ARN) of a cached volume. All of the specified cached volumes must be from the same gateway. Use 'ListVolumes' to get volume ARNs for a gateway.
--
-- /Note:/ Consider using 'volumeARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscsivVolumeARNs :: Lens.Lens' DescribeCachediSCSIVolumes [Lude.Text]
dcscsivVolumeARNs = Lens.lens (volumeARNs :: DescribeCachediSCSIVolumes -> [Lude.Text]) (\s a -> s {volumeARNs = a} :: DescribeCachediSCSIVolumes)
{-# DEPRECATED dcscsivVolumeARNs "Use generic-lens or generic-optics with 'volumeARNs' instead." #-}

instance Lude.AWSRequest DescribeCachediSCSIVolumes where
  type
    Rs DescribeCachediSCSIVolumes =
      DescribeCachediSCSIVolumesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCachediSCSIVolumesResponse'
            Lude.<$> (x Lude..?> "CachediSCSIVolumes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCachediSCSIVolumes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeCachediSCSIVolumes" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCachediSCSIVolumes where
  toJSON DescribeCachediSCSIVolumes' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("VolumeARNs" Lude..= volumeARNs)])

instance Lude.ToPath DescribeCachediSCSIVolumes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCachediSCSIVolumes where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'mkDescribeCachediSCSIVolumesResponse' smart constructor.
data DescribeCachediSCSIVolumesResponse = DescribeCachediSCSIVolumesResponse'
  { cachediSCSIVolumes ::
      Lude.Maybe
        [CachediSCSIVolume],
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCachediSCSIVolumesResponse' with the minimum fields required to make a request.
--
-- * 'cachediSCSIVolumes' - An array of objects where each object contains metadata about one cached volume.
-- * 'responseStatus' - The response status code.
mkDescribeCachediSCSIVolumesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCachediSCSIVolumesResponse
mkDescribeCachediSCSIVolumesResponse pResponseStatus_ =
  DescribeCachediSCSIVolumesResponse'
    { cachediSCSIVolumes =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects where each object contains metadata about one cached volume.
--
-- /Note:/ Consider using 'cachediSCSIVolumes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscsivrsCachediSCSIVolumes :: Lens.Lens' DescribeCachediSCSIVolumesResponse (Lude.Maybe [CachediSCSIVolume])
dcscsivrsCachediSCSIVolumes = Lens.lens (cachediSCSIVolumes :: DescribeCachediSCSIVolumesResponse -> Lude.Maybe [CachediSCSIVolume]) (\s a -> s {cachediSCSIVolumes = a} :: DescribeCachediSCSIVolumesResponse)
{-# DEPRECATED dcscsivrsCachediSCSIVolumes "Use generic-lens or generic-optics with 'cachediSCSIVolumes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcscsivrsResponseStatus :: Lens.Lens' DescribeCachediSCSIVolumesResponse Lude.Int
dcscsivrsResponseStatus = Lens.lens (responseStatus :: DescribeCachediSCSIVolumesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCachediSCSIVolumesResponse)
{-# DEPRECATED dcscsivrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
