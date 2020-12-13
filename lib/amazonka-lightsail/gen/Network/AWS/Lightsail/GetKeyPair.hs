{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetKeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific key pair.
module Network.AWS.Lightsail.GetKeyPair
  ( -- * Creating a request
    GetKeyPair (..),
    mkGetKeyPair,

    -- ** Request lenses
    gkpKeyPairName,

    -- * Destructuring the response
    GetKeyPairResponse (..),
    mkGetKeyPairResponse,

    -- ** Response lenses
    gkprsKeyPair,
    gkprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetKeyPair' smart constructor.
newtype GetKeyPair = GetKeyPair'
  { -- | The name of the key pair for which you are requesting information.
    keyPairName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetKeyPair' with the minimum fields required to make a request.
--
-- * 'keyPairName' - The name of the key pair for which you are requesting information.
mkGetKeyPair ::
  -- | 'keyPairName'
  Lude.Text ->
  GetKeyPair
mkGetKeyPair pKeyPairName_ =
  GetKeyPair' {keyPairName = pKeyPairName_}

-- | The name of the key pair for which you are requesting information.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkpKeyPairName :: Lens.Lens' GetKeyPair Lude.Text
gkpKeyPairName = Lens.lens (keyPairName :: GetKeyPair -> Lude.Text) (\s a -> s {keyPairName = a} :: GetKeyPair)
{-# DEPRECATED gkpKeyPairName "Use generic-lens or generic-optics with 'keyPairName' instead." #-}

instance Lude.AWSRequest GetKeyPair where
  type Rs GetKeyPair = GetKeyPairResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetKeyPairResponse'
            Lude.<$> (x Lude..?> "keyPair") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetKeyPair where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetKeyPair" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetKeyPair where
  toJSON GetKeyPair' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("keyPairName" Lude..= keyPairName)])

instance Lude.ToPath GetKeyPair where
  toPath = Lude.const "/"

instance Lude.ToQuery GetKeyPair where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetKeyPairResponse' smart constructor.
data GetKeyPairResponse = GetKeyPairResponse'
  { -- | An array of key-value pairs containing information about the key pair.
    keyPair :: Lude.Maybe KeyPair,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetKeyPairResponse' with the minimum fields required to make a request.
--
-- * 'keyPair' - An array of key-value pairs containing information about the key pair.
-- * 'responseStatus' - The response status code.
mkGetKeyPairResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetKeyPairResponse
mkGetKeyPairResponse pResponseStatus_ =
  GetKeyPairResponse'
    { keyPair = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of key-value pairs containing information about the key pair.
--
-- /Note:/ Consider using 'keyPair' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprsKeyPair :: Lens.Lens' GetKeyPairResponse (Lude.Maybe KeyPair)
gkprsKeyPair = Lens.lens (keyPair :: GetKeyPairResponse -> Lude.Maybe KeyPair) (\s a -> s {keyPair = a} :: GetKeyPairResponse)
{-# DEPRECATED gkprsKeyPair "Use generic-lens or generic-optics with 'keyPair' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gkprsResponseStatus :: Lens.Lens' GetKeyPairResponse Lude.Int
gkprsResponseStatus = Lens.lens (responseStatus :: GetKeyPairResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetKeyPairResponse)
{-# DEPRECATED gkprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
