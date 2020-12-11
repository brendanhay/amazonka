{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DeletePlatformVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of a custom platform.
module Network.AWS.ElasticBeanstalk.DeletePlatformVersion
  ( -- * Creating a request
    DeletePlatformVersion (..),
    mkDeletePlatformVersion,

    -- ** Request lenses
    dpvPlatformARN,

    -- * Destructuring the response
    DeletePlatformVersionResponse (..),
    mkDeletePlatformVersionResponse,

    -- ** Response lenses
    dpvrsPlatformSummary,
    dpvrsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePlatformVersion' smart constructor.
newtype DeletePlatformVersion = DeletePlatformVersion'
  { platformARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePlatformVersion' with the minimum fields required to make a request.
--
-- * 'platformARN' - The ARN of the version of the custom platform.
mkDeletePlatformVersion ::
  DeletePlatformVersion
mkDeletePlatformVersion =
  DeletePlatformVersion' {platformARN = Lude.Nothing}

-- | The ARN of the version of the custom platform.
--
-- /Note:/ Consider using 'platformARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvPlatformARN :: Lens.Lens' DeletePlatformVersion (Lude.Maybe Lude.Text)
dpvPlatformARN = Lens.lens (platformARN :: DeletePlatformVersion -> Lude.Maybe Lude.Text) (\s a -> s {platformARN = a} :: DeletePlatformVersion)
{-# DEPRECATED dpvPlatformARN "Use generic-lens or generic-optics with 'platformARN' instead." #-}

instance Lude.AWSRequest DeletePlatformVersion where
  type Rs DeletePlatformVersion = DeletePlatformVersionResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "DeletePlatformVersionResult"
      ( \s h x ->
          DeletePlatformVersionResponse'
            Lude.<$> (x Lude..@? "PlatformSummary")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeletePlatformVersion where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePlatformVersion where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePlatformVersion where
  toQuery DeletePlatformVersion' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeletePlatformVersion" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "PlatformArn" Lude.=: platformARN
      ]

-- | /See:/ 'mkDeletePlatformVersionResponse' smart constructor.
data DeletePlatformVersionResponse = DeletePlatformVersionResponse'
  { platformSummary ::
      Lude.Maybe PlatformSummary,
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

-- | Creates a value of 'DeletePlatformVersionResponse' with the minimum fields required to make a request.
--
-- * 'platformSummary' - Detailed information about the version of the custom platform.
-- * 'responseStatus' - The response status code.
mkDeletePlatformVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeletePlatformVersionResponse
mkDeletePlatformVersionResponse pResponseStatus_ =
  DeletePlatformVersionResponse'
    { platformSummary = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Detailed information about the version of the custom platform.
--
-- /Note:/ Consider using 'platformSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrsPlatformSummary :: Lens.Lens' DeletePlatformVersionResponse (Lude.Maybe PlatformSummary)
dpvrsPlatformSummary = Lens.lens (platformSummary :: DeletePlatformVersionResponse -> Lude.Maybe PlatformSummary) (\s a -> s {platformSummary = a} :: DeletePlatformVersionResponse)
{-# DEPRECATED dpvrsPlatformSummary "Use generic-lens or generic-optics with 'platformSummary' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpvrsResponseStatus :: Lens.Lens' DeletePlatformVersionResponse Lude.Int
dpvrsResponseStatus = Lens.lens (responseStatus :: DeletePlatformVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeletePlatformVersionResponse)
{-# DEPRECATED dpvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
