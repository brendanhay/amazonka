{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.DisableDomainAutoRenew
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation disables automatic renewal of domain registration for the specified domain.
module Network.AWS.Route53Domains.DisableDomainAutoRenew
  ( -- * Creating a request
    DisableDomainAutoRenew (..),
    mkDisableDomainAutoRenew,

    -- ** Request lenses
    ddarDomainName,

    -- * Destructuring the response
    DisableDomainAutoRenewResponse (..),
    mkDisableDomainAutoRenewResponse,

    -- ** Response lenses
    ddarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | /See:/ 'mkDisableDomainAutoRenew' smart constructor.
newtype DisableDomainAutoRenew = DisableDomainAutoRenew'
  { -- | The name of the domain that you want to disable automatic renewal for.
    domainName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableDomainAutoRenew' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to disable automatic renewal for.
mkDisableDomainAutoRenew ::
  -- | 'domainName'
  Lude.Text ->
  DisableDomainAutoRenew
mkDisableDomainAutoRenew pDomainName_ =
  DisableDomainAutoRenew' {domainName = pDomainName_}

-- | The name of the domain that you want to disable automatic renewal for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddarDomainName :: Lens.Lens' DisableDomainAutoRenew Lude.Text
ddarDomainName = Lens.lens (domainName :: DisableDomainAutoRenew -> Lude.Text) (\s a -> s {domainName = a} :: DisableDomainAutoRenew)
{-# DEPRECATED ddarDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest DisableDomainAutoRenew where
  type Rs DisableDomainAutoRenew = DisableDomainAutoRenewResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisableDomainAutoRenewResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableDomainAutoRenew where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.DisableDomainAutoRenew" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableDomainAutoRenew where
  toJSON DisableDomainAutoRenew' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath DisableDomainAutoRenew where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableDomainAutoRenew where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableDomainAutoRenewResponse' smart constructor.
newtype DisableDomainAutoRenewResponse = DisableDomainAutoRenewResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableDomainAutoRenewResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisableDomainAutoRenewResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableDomainAutoRenewResponse
mkDisableDomainAutoRenewResponse pResponseStatus_ =
  DisableDomainAutoRenewResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddarrsResponseStatus :: Lens.Lens' DisableDomainAutoRenewResponse Lude.Int
ddarrsResponseStatus = Lens.lens (responseStatus :: DisableDomainAutoRenewResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableDomainAutoRenewResponse)
{-# DEPRECATED ddarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
