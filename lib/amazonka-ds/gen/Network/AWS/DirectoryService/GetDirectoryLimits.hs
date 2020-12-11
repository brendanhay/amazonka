{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.GetDirectoryLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains directory limit information for the current Region.
module Network.AWS.DirectoryService.GetDirectoryLimits
  ( -- * Creating a request
    GetDirectoryLimits (..),
    mkGetDirectoryLimits,

    -- * Destructuring the response
    GetDirectoryLimitsResponse (..),
    mkGetDirectoryLimitsResponse,

    -- ** Response lenses
    gdlrsDirectoryLimits,
    gdlrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'GetDirectoryLimits' operation.
--
-- /See:/ 'mkGetDirectoryLimits' smart constructor.
data GetDirectoryLimits = GetDirectoryLimits'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDirectoryLimits' with the minimum fields required to make a request.
mkGetDirectoryLimits ::
  GetDirectoryLimits
mkGetDirectoryLimits = GetDirectoryLimits'

instance Lude.AWSRequest GetDirectoryLimits where
  type Rs GetDirectoryLimits = GetDirectoryLimitsResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDirectoryLimitsResponse'
            Lude.<$> (x Lude..?> "DirectoryLimits")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDirectoryLimits where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.GetDirectoryLimits" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDirectoryLimits where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetDirectoryLimits where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDirectoryLimits where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'GetDirectoryLimits' operation.
--
-- /See:/ 'mkGetDirectoryLimitsResponse' smart constructor.
data GetDirectoryLimitsResponse = GetDirectoryLimitsResponse'
  { directoryLimits ::
      Lude.Maybe DirectoryLimits,
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

-- | Creates a value of 'GetDirectoryLimitsResponse' with the minimum fields required to make a request.
--
-- * 'directoryLimits' - A 'DirectoryLimits' object that contains the directory limits for the current rRegion.
-- * 'responseStatus' - The response status code.
mkGetDirectoryLimitsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDirectoryLimitsResponse
mkGetDirectoryLimitsResponse pResponseStatus_ =
  GetDirectoryLimitsResponse'
    { directoryLimits = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A 'DirectoryLimits' object that contains the directory limits for the current rRegion.
--
-- /Note:/ Consider using 'directoryLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlrsDirectoryLimits :: Lens.Lens' GetDirectoryLimitsResponse (Lude.Maybe DirectoryLimits)
gdlrsDirectoryLimits = Lens.lens (directoryLimits :: GetDirectoryLimitsResponse -> Lude.Maybe DirectoryLimits) (\s a -> s {directoryLimits = a} :: GetDirectoryLimitsResponse)
{-# DEPRECATED gdlrsDirectoryLimits "Use generic-lens or generic-optics with 'directoryLimits' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdlrsResponseStatus :: Lens.Lens' GetDirectoryLimitsResponse Lude.Int
gdlrsResponseStatus = Lens.lens (responseStatus :: GetDirectoryLimitsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDirectoryLimitsResponse)
{-# DEPRECATED gdlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
