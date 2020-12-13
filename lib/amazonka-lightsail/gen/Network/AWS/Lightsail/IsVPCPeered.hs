{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.IsVPCPeered
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a Boolean value indicating whether your Lightsail VPC is peered.
module Network.AWS.Lightsail.IsVPCPeered
  ( -- * Creating a request
    IsVPCPeered (..),
    mkIsVPCPeered,

    -- * Destructuring the response
    IsVPCPeeredResponse (..),
    mkIsVPCPeeredResponse,

    -- ** Response lenses
    ivprsIsPeered,
    ivprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkIsVPCPeered' smart constructor.
data IsVPCPeered = IsVPCPeered'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IsVPCPeered' with the minimum fields required to make a request.
mkIsVPCPeered ::
  IsVPCPeered
mkIsVPCPeered = IsVPCPeered'

instance Lude.AWSRequest IsVPCPeered where
  type Rs IsVPCPeered = IsVPCPeeredResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          IsVPCPeeredResponse'
            Lude.<$> (x Lude..?> "isPeered") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders IsVPCPeered where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.IsVpcPeered" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON IsVPCPeered where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath IsVPCPeered where
  toPath = Lude.const "/"

instance Lude.ToQuery IsVPCPeered where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkIsVPCPeeredResponse' smart constructor.
data IsVPCPeeredResponse = IsVPCPeeredResponse'
  { -- | Returns @true@ if the Lightsail VPC is peered; otherwise, @false@ .
    isPeered :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IsVPCPeeredResponse' with the minimum fields required to make a request.
--
-- * 'isPeered' - Returns @true@ if the Lightsail VPC is peered; otherwise, @false@ .
-- * 'responseStatus' - The response status code.
mkIsVPCPeeredResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  IsVPCPeeredResponse
mkIsVPCPeeredResponse pResponseStatus_ =
  IsVPCPeeredResponse'
    { isPeered = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the Lightsail VPC is peered; otherwise, @false@ .
--
-- /Note:/ Consider using 'isPeered' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivprsIsPeered :: Lens.Lens' IsVPCPeeredResponse (Lude.Maybe Lude.Bool)
ivprsIsPeered = Lens.lens (isPeered :: IsVPCPeeredResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isPeered = a} :: IsVPCPeeredResponse)
{-# DEPRECATED ivprsIsPeered "Use generic-lens or generic-optics with 'isPeered' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ivprsResponseStatus :: Lens.Lens' IsVPCPeeredResponse Lude.Int
ivprsResponseStatus = Lens.lens (responseStatus :: IsVPCPeeredResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: IsVPCPeeredResponse)
{-# DEPRECATED ivprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
