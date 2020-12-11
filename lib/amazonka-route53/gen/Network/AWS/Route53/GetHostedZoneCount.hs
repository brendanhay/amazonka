{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetHostedZoneCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the number of hosted zones that are associated with the current AWS account.
module Network.AWS.Route53.GetHostedZoneCount
  ( -- * Creating a request
    GetHostedZoneCount (..),
    mkGetHostedZoneCount,

    -- * Destructuring the response
    GetHostedZoneCountResponse (..),
    mkGetHostedZoneCountResponse,

    -- ** Response lenses
    ghzcrsResponseStatus,
    ghzcrsHostedZoneCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to retrieve a count of all the hosted zones that are associated with the current AWS account.
--
-- /See:/ 'mkGetHostedZoneCount' smart constructor.
data GetHostedZoneCount = GetHostedZoneCount'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHostedZoneCount' with the minimum fields required to make a request.
mkGetHostedZoneCount ::
  GetHostedZoneCount
mkGetHostedZoneCount = GetHostedZoneCount'

instance Lude.AWSRequest GetHostedZoneCount where
  type Rs GetHostedZoneCount = GetHostedZoneCountResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetHostedZoneCountResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "HostedZoneCount")
      )

instance Lude.ToHeaders GetHostedZoneCount where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetHostedZoneCount where
  toPath = Lude.const "/2013-04-01/hostedzonecount"

instance Lude.ToQuery GetHostedZoneCount where
  toQuery = Lude.const Lude.mempty

-- | A complex type that contains the response to a @GetHostedZoneCount@ request.
--
-- /See:/ 'mkGetHostedZoneCountResponse' smart constructor.
data GetHostedZoneCountResponse = GetHostedZoneCountResponse'
  { responseStatus ::
      Lude.Int,
    hostedZoneCount :: Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetHostedZoneCountResponse' with the minimum fields required to make a request.
--
-- * 'hostedZoneCount' - The total number of public and private hosted zones that are associated with the current AWS account.
-- * 'responseStatus' - The response status code.
mkGetHostedZoneCountResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'hostedZoneCount'
  Lude.Integer ->
  GetHostedZoneCountResponse
mkGetHostedZoneCountResponse pResponseStatus_ pHostedZoneCount_ =
  GetHostedZoneCountResponse'
    { responseStatus = pResponseStatus_,
      hostedZoneCount = pHostedZoneCount_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzcrsResponseStatus :: Lens.Lens' GetHostedZoneCountResponse Lude.Int
ghzcrsResponseStatus = Lens.lens (responseStatus :: GetHostedZoneCountResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetHostedZoneCountResponse)
{-# DEPRECATED ghzcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The total number of public and private hosted zones that are associated with the current AWS account.
--
-- /Note:/ Consider using 'hostedZoneCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ghzcrsHostedZoneCount :: Lens.Lens' GetHostedZoneCountResponse Lude.Integer
ghzcrsHostedZoneCount = Lens.lens (hostedZoneCount :: GetHostedZoneCountResponse -> Lude.Integer) (\s a -> s {hostedZoneCount = a} :: GetHostedZoneCountResponse)
{-# DEPRECATED ghzcrsHostedZoneCount "Use generic-lens or generic-optics with 'hostedZoneCount' instead." #-}
