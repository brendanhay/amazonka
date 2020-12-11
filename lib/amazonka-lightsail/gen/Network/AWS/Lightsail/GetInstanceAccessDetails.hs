{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetInstanceAccessDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns temporary SSH keys you can use to connect to a specific virtual private server, or /instance/ .
--
-- The @get instance access details@ operation supports tag-based access control via resource tags applied to the resource identified by @instance name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.GetInstanceAccessDetails
  ( -- * Creating a request
    GetInstanceAccessDetails (..),
    mkGetInstanceAccessDetails,

    -- ** Request lenses
    giadProtocol,
    giadInstanceName,

    -- * Destructuring the response
    GetInstanceAccessDetailsResponse (..),
    mkGetInstanceAccessDetailsResponse,

    -- ** Response lenses
    giadrsAccessDetails,
    giadrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetInstanceAccessDetails' smart constructor.
data GetInstanceAccessDetails = GetInstanceAccessDetails'
  { protocol ::
      Lude.Maybe InstanceAccessProtocol,
    instanceName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetInstanceAccessDetails' with the minimum fields required to make a request.
--
-- * 'instanceName' - The name of the instance to access.
-- * 'protocol' - The protocol to use to connect to your instance. Defaults to @ssh@ .
mkGetInstanceAccessDetails ::
  -- | 'instanceName'
  Lude.Text ->
  GetInstanceAccessDetails
mkGetInstanceAccessDetails pInstanceName_ =
  GetInstanceAccessDetails'
    { protocol = Lude.Nothing,
      instanceName = pInstanceName_
    }

-- | The protocol to use to connect to your instance. Defaults to @ssh@ .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giadProtocol :: Lens.Lens' GetInstanceAccessDetails (Lude.Maybe InstanceAccessProtocol)
giadProtocol = Lens.lens (protocol :: GetInstanceAccessDetails -> Lude.Maybe InstanceAccessProtocol) (\s a -> s {protocol = a} :: GetInstanceAccessDetails)
{-# DEPRECATED giadProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The name of the instance to access.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giadInstanceName :: Lens.Lens' GetInstanceAccessDetails Lude.Text
giadInstanceName = Lens.lens (instanceName :: GetInstanceAccessDetails -> Lude.Text) (\s a -> s {instanceName = a} :: GetInstanceAccessDetails)
{-# DEPRECATED giadInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest GetInstanceAccessDetails where
  type Rs GetInstanceAccessDetails = GetInstanceAccessDetailsResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetInstanceAccessDetailsResponse'
            Lude.<$> (x Lude..?> "accessDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetInstanceAccessDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetInstanceAccessDetails" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetInstanceAccessDetails where
  toJSON GetInstanceAccessDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("protocol" Lude..=) Lude.<$> protocol,
            Lude.Just ("instanceName" Lude..= instanceName)
          ]
      )

instance Lude.ToPath GetInstanceAccessDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery GetInstanceAccessDetails where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetInstanceAccessDetailsResponse' smart constructor.
data GetInstanceAccessDetailsResponse = GetInstanceAccessDetailsResponse'
  { accessDetails ::
      Lude.Maybe
        InstanceAccessDetails,
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

-- | Creates a value of 'GetInstanceAccessDetailsResponse' with the minimum fields required to make a request.
--
-- * 'accessDetails' - An array of key-value pairs containing information about a get instance access request.
-- * 'responseStatus' - The response status code.
mkGetInstanceAccessDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetInstanceAccessDetailsResponse
mkGetInstanceAccessDetailsResponse pResponseStatus_ =
  GetInstanceAccessDetailsResponse'
    { accessDetails = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of key-value pairs containing information about a get instance access request.
--
-- /Note:/ Consider using 'accessDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giadrsAccessDetails :: Lens.Lens' GetInstanceAccessDetailsResponse (Lude.Maybe InstanceAccessDetails)
giadrsAccessDetails = Lens.lens (accessDetails :: GetInstanceAccessDetailsResponse -> Lude.Maybe InstanceAccessDetails) (\s a -> s {accessDetails = a} :: GetInstanceAccessDetailsResponse)
{-# DEPRECATED giadrsAccessDetails "Use generic-lens or generic-optics with 'accessDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giadrsResponseStatus :: Lens.Lens' GetInstanceAccessDetailsResponse Lude.Int
giadrsResponseStatus = Lens.lens (responseStatus :: GetInstanceAccessDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetInstanceAccessDetailsResponse)
{-# DEPRECATED giadrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
