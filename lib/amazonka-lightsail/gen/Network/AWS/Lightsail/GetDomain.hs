{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetDomain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific domain recordset.
module Network.AWS.Lightsail.GetDomain
  ( -- * Creating a request
    GetDomain (..),
    mkGetDomain,

    -- ** Request lenses
    gdDomainName,

    -- * Destructuring the response
    GetDomainResponse (..),
    mkGetDomainResponse,

    -- ** Response lenses
    gdfrsDomain,
    gdfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetDomain' smart constructor.
newtype GetDomain = GetDomain'
  { -- | The domain name for which your want to return information about.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDomain' with the minimum fields required to make a request.
--
-- * 'domainName' - The domain name for which your want to return information about.
mkGetDomain ::
  -- | 'domainName'
  Lude.Text ->
  GetDomain
mkGetDomain pDomainName_ = GetDomain' {domainName = pDomainName_}

-- | The domain name for which your want to return information about.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDomainName :: Lens.Lens' GetDomain Lude.Text
gdDomainName = Lens.lens (domainName :: GetDomain -> Lude.Text) (\s a -> s {domainName = a} :: GetDomain)
{-# DEPRECATED gdDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest GetDomain where
  type Rs GetDomain = GetDomainResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDomainResponse'
            Lude.<$> (x Lude..?> "domain") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDomain where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.GetDomain" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetDomain where
  toJSON GetDomain' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("domainName" Lude..= domainName)])

instance Lude.ToPath GetDomain where
  toPath = Lude.const "/"

instance Lude.ToQuery GetDomain where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetDomainResponse' smart constructor.
data GetDomainResponse = GetDomainResponse'
  { -- | An array of key-value pairs containing information about your get domain request.
    domain :: Lude.Maybe Domain,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDomainResponse' with the minimum fields required to make a request.
--
-- * 'domain' - An array of key-value pairs containing information about your get domain request.
-- * 'responseStatus' - The response status code.
mkGetDomainResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDomainResponse
mkGetDomainResponse pResponseStatus_ =
  GetDomainResponse'
    { domain = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of key-value pairs containing information about your get domain request.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdfrsDomain :: Lens.Lens' GetDomainResponse (Lude.Maybe Domain)
gdfrsDomain = Lens.lens (domain :: GetDomainResponse -> Lude.Maybe Domain) (\s a -> s {domain = a} :: GetDomainResponse)
{-# DEPRECATED gdfrsDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdfrsResponseStatus :: Lens.Lens' GetDomainResponse Lude.Int
gdfrsResponseStatus = Lens.lens (responseStatus :: GetDomainResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDomainResponse)
{-# DEPRECATED gdfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
