{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetStaticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific static IP.
module Network.AWS.Lightsail.GetStaticIP
  ( -- * Creating a request
    GetStaticIP (..),
    mkGetStaticIP,

    -- ** Request lenses
    gsiStaticIPName,

    -- * Destructuring the response
    GetStaticIPResponse (..),
    mkGetStaticIPResponse,

    -- ** Response lenses
    gsirsStaticIP,
    gsirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetStaticIP' smart constructor.
newtype GetStaticIP = GetStaticIP' {staticIPName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStaticIP' with the minimum fields required to make a request.
--
-- * 'staticIPName' - The name of the static IP in Lightsail.
mkGetStaticIP ::
  -- | 'staticIPName'
  Lude.Text ->
  GetStaticIP
mkGetStaticIP pStaticIPName_ =
  GetStaticIP' {staticIPName = pStaticIPName_}

-- | The name of the static IP in Lightsail.
--
-- /Note:/ Consider using 'staticIPName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsiStaticIPName :: Lens.Lens' GetStaticIP Lude.Text
gsiStaticIPName = Lens.lens (staticIPName :: GetStaticIP -> Lude.Text) (\s a -> s {staticIPName = a} :: GetStaticIP)
{-# DEPRECATED gsiStaticIPName "Use generic-lens or generic-optics with 'staticIPName' instead." #-}

instance Lude.AWSRequest GetStaticIP where
  type Rs GetStaticIP = GetStaticIPResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetStaticIPResponse'
            Lude.<$> (x Lude..?> "staticIp") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetStaticIP where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetStaticIp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetStaticIP where
  toJSON GetStaticIP' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("staticIpName" Lude..= staticIPName)])

instance Lude.ToPath GetStaticIP where
  toPath = Lude.const "/"

instance Lude.ToQuery GetStaticIP where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetStaticIPResponse' smart constructor.
data GetStaticIPResponse = GetStaticIPResponse'
  { staticIP ::
      Lude.Maybe StaticIP,
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

-- | Creates a value of 'GetStaticIPResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'staticIP' - An array of key-value pairs containing information about the requested static IP.
mkGetStaticIPResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetStaticIPResponse
mkGetStaticIPResponse pResponseStatus_ =
  GetStaticIPResponse'
    { staticIP = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of key-value pairs containing information about the requested static IP.
--
-- /Note:/ Consider using 'staticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirsStaticIP :: Lens.Lens' GetStaticIPResponse (Lude.Maybe StaticIP)
gsirsStaticIP = Lens.lens (staticIP :: GetStaticIPResponse -> Lude.Maybe StaticIP) (\s a -> s {staticIP = a} :: GetStaticIPResponse)
{-# DEPRECATED gsirsStaticIP "Use generic-lens or generic-optics with 'staticIP' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsirsResponseStatus :: Lens.Lens' GetStaticIPResponse Lude.Int
gsirsResponseStatus = Lens.lens (responseStatus :: GetStaticIPResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetStaticIPResponse)
{-# DEPRECATED gsirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
