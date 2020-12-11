{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudTrail.GetTrail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns settings information for a specified trail.
module Network.AWS.CloudTrail.GetTrail
  ( -- * Creating a request
    GetTrail (..),
    mkGetTrail,

    -- ** Request lenses
    gtName,

    -- * Destructuring the response
    GetTrailResponse (..),
    mkGetTrailResponse,

    -- ** Response lenses
    gtrsTrail,
    gtrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTrail' smart constructor.
newtype GetTrail = GetTrail' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTrail' with the minimum fields required to make a request.
--
-- * 'name' - The name or the Amazon Resource Name (ARN) of the trail for which you want to retrieve settings information.
mkGetTrail ::
  -- | 'name'
  Lude.Text ->
  GetTrail
mkGetTrail pName_ = GetTrail' {name = pName_}

-- | The name or the Amazon Resource Name (ARN) of the trail for which you want to retrieve settings information.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtName :: Lens.Lens' GetTrail Lude.Text
gtName = Lens.lens (name :: GetTrail -> Lude.Text) (\s a -> s {name = a} :: GetTrail)
{-# DEPRECATED gtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetTrail where
  type Rs GetTrail = GetTrailResponse
  request = Req.postJSON cloudTrailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTrailResponse'
            Lude.<$> (x Lude..?> "Trail") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTrail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetTrail" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTrail where
  toJSON GetTrail' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath GetTrail where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTrail where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTrailResponse' smart constructor.
data GetTrailResponse = GetTrailResponse'
  { trail ::
      Lude.Maybe Trail,
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

-- | Creates a value of 'GetTrailResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trail' - Undocumented field.
mkGetTrailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTrailResponse
mkGetTrailResponse pResponseStatus_ =
  GetTrailResponse'
    { trail = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'trail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsTrail :: Lens.Lens' GetTrailResponse (Lude.Maybe Trail)
gtrsTrail = Lens.lens (trail :: GetTrailResponse -> Lude.Maybe Trail) (\s a -> s {trail = a} :: GetTrailResponse)
{-# DEPRECATED gtrsTrail "Use generic-lens or generic-optics with 'trail' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtrsResponseStatus :: Lens.Lens' GetTrailResponse Lude.Int
gtrsResponseStatus = Lens.lens (responseStatus :: GetTrailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTrailResponse)
{-# DEPRECATED gtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
