{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.EnableDomainAutoRenew
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation configures Amazon Route 53 to automatically renew the specified domain before the domain registration expires. The cost of renewing your domain registration is billed to your AWS account.
--
-- The period during which you can renew a domain name varies by TLD. For a list of TLDs and their renewal policies, see <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/registrar-tld-list.html Domains That You Can Register with Amazon Route 53> in the /Amazon Route 53 Developer Guide/ . Route 53 requires that you renew before the end of the renewal period so we can complete processing before the deadline.
module Network.AWS.Route53Domains.EnableDomainAutoRenew
  ( -- * Creating a request
    EnableDomainAutoRenew (..),
    mkEnableDomainAutoRenew,

    -- ** Request lenses
    edarDomainName,

    -- * Destructuring the response
    EnableDomainAutoRenewResponse (..),
    mkEnableDomainAutoRenewResponse,

    -- ** Response lenses
    edarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | /See:/ 'mkEnableDomainAutoRenew' smart constructor.
newtype EnableDomainAutoRenew = EnableDomainAutoRenew'
  { domainName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableDomainAutoRenew' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to enable automatic renewal for.
mkEnableDomainAutoRenew ::
  -- | 'domainName'
  Lude.Text ->
  EnableDomainAutoRenew
mkEnableDomainAutoRenew pDomainName_ =
  EnableDomainAutoRenew' {domainName = pDomainName_}

-- | The name of the domain that you want to enable automatic renewal for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edarDomainName :: Lens.Lens' EnableDomainAutoRenew Lude.Text
edarDomainName = Lens.lens (domainName :: EnableDomainAutoRenew -> Lude.Text) (\s a -> s {domainName = a} :: EnableDomainAutoRenew)
{-# DEPRECATED edarDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest EnableDomainAutoRenew where
  type Rs EnableDomainAutoRenew = EnableDomainAutoRenewResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          EnableDomainAutoRenewResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableDomainAutoRenew where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.EnableDomainAutoRenew" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableDomainAutoRenew where
  toJSON EnableDomainAutoRenew' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("DomainName" Lude..= domainName)])

instance Lude.ToPath EnableDomainAutoRenew where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableDomainAutoRenew where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableDomainAutoRenewResponse' smart constructor.
newtype EnableDomainAutoRenewResponse = EnableDomainAutoRenewResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableDomainAutoRenewResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkEnableDomainAutoRenewResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableDomainAutoRenewResponse
mkEnableDomainAutoRenewResponse pResponseStatus_ =
  EnableDomainAutoRenewResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edarrsResponseStatus :: Lens.Lens' EnableDomainAutoRenewResponse Lude.Int
edarrsResponseStatus = Lens.lens (responseStatus :: EnableDomainAutoRenewResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableDomainAutoRenewResponse)
{-# DEPRECATED edarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
