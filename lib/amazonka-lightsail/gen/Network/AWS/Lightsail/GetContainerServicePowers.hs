{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerServicePowers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of powers that can be specified for your Amazon Lightsail container services.
--
-- The power specifies the amount of memory, the number of vCPUs, and the base price of the container service.
module Network.AWS.Lightsail.GetContainerServicePowers
  ( -- * Creating a request
    GetContainerServicePowers (..),
    mkGetContainerServicePowers,

    -- * Destructuring the response
    GetContainerServicePowersResponse (..),
    mkGetContainerServicePowersResponse,

    -- ** Response lenses
    gcsprsPowers,
    gcsprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContainerServicePowers' smart constructor.
data GetContainerServicePowers = GetContainerServicePowers'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContainerServicePowers' with the minimum fields required to make a request.
mkGetContainerServicePowers ::
  GetContainerServicePowers
mkGetContainerServicePowers = GetContainerServicePowers'

instance Lude.AWSRequest GetContainerServicePowers where
  type
    Rs GetContainerServicePowers =
      GetContainerServicePowersResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContainerServicePowersResponse'
            Lude.<$> (x Lude..?> "powers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContainerServicePowers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetContainerServicePowers" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContainerServicePowers where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath GetContainerServicePowers where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContainerServicePowers where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContainerServicePowersResponse' smart constructor.
data GetContainerServicePowersResponse = GetContainerServicePowersResponse'
  { powers ::
      Lude.Maybe
        [ContainerServicePower],
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

-- | Creates a value of 'GetContainerServicePowersResponse' with the minimum fields required to make a request.
--
-- * 'powers' - An array of objects that describe the powers that can be specified for a container service.
-- * 'responseStatus' - The response status code.
mkGetContainerServicePowersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContainerServicePowersResponse
mkGetContainerServicePowersResponse pResponseStatus_ =
  GetContainerServicePowersResponse'
    { powers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the powers that can be specified for a container service.
--
-- /Note:/ Consider using 'powers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsprsPowers :: Lens.Lens' GetContainerServicePowersResponse (Lude.Maybe [ContainerServicePower])
gcsprsPowers = Lens.lens (powers :: GetContainerServicePowersResponse -> Lude.Maybe [ContainerServicePower]) (\s a -> s {powers = a} :: GetContainerServicePowersResponse)
{-# DEPRECATED gcsprsPowers "Use generic-lens or generic-optics with 'powers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsprsResponseStatus :: Lens.Lens' GetContainerServicePowersResponse Lude.Int
gcsprsResponseStatus = Lens.lens (responseStatus :: GetContainerServicePowersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContainerServicePowersResponse)
{-# DEPRECATED gcsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
