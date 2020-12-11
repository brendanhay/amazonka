{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DescribeStudio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details for the specified Amazon EMR Studio including ID, Name, VPC, Studio access URL, and so on.
module Network.AWS.EMR.DescribeStudio
  ( -- * Creating a request
    DescribeStudio (..),
    mkDescribeStudio,

    -- ** Request lenses
    dsStudioId,

    -- * Destructuring the response
    DescribeStudioResponse (..),
    mkDescribeStudioResponse,

    -- ** Response lenses
    desrsStudio,
    desrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStudio' smart constructor.
newtype DescribeStudio = DescribeStudio' {studioId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStudio' with the minimum fields required to make a request.
--
-- * 'studioId' - The Amazon EMR Studio ID.
mkDescribeStudio ::
  -- | 'studioId'
  Lude.Text ->
  DescribeStudio
mkDescribeStudio pStudioId_ =
  DescribeStudio' {studioId = pStudioId_}

-- | The Amazon EMR Studio ID.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsStudioId :: Lens.Lens' DescribeStudio Lude.Text
dsStudioId = Lens.lens (studioId :: DescribeStudio -> Lude.Text) (\s a -> s {studioId = a} :: DescribeStudio)
{-# DEPRECATED dsStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

instance Lude.AWSRequest DescribeStudio where
  type Rs DescribeStudio = DescribeStudioResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeStudioResponse'
            Lude.<$> (x Lude..?> "Studio") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStudio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.DescribeStudio" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeStudio where
  toJSON DescribeStudio' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("StudioId" Lude..= studioId)])

instance Lude.ToPath DescribeStudio where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStudio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeStudioResponse' smart constructor.
data DescribeStudioResponse = DescribeStudioResponse'
  { studio ::
      Lude.Maybe Studio,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStudioResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'studio' - The Amazon EMR Studio details.
mkDescribeStudioResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStudioResponse
mkDescribeStudioResponse pResponseStatus_ =
  DescribeStudioResponse'
    { studio = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon EMR Studio details.
--
-- /Note:/ Consider using 'studio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsStudio :: Lens.Lens' DescribeStudioResponse (Lude.Maybe Studio)
desrsStudio = Lens.lens (studio :: DescribeStudioResponse -> Lude.Maybe Studio) (\s a -> s {studio = a} :: DescribeStudioResponse)
{-# DEPRECATED desrsStudio "Use generic-lens or generic-optics with 'studio' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeStudioResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeStudioResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStudioResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
