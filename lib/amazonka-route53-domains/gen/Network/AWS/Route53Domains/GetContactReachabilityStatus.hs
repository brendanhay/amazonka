{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.GetContactReachabilityStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For operations that require confirmation that the email address for the registrant contact is valid, such as registering a new domain, this operation returns information about whether the registrant contact has responded.
--
-- If you want us to resend the email, use the @ResendContactReachabilityEmail@ operation.
module Network.AWS.Route53Domains.GetContactReachabilityStatus
  ( -- * Creating a request
    GetContactReachabilityStatus (..),
    mkGetContactReachabilityStatus,

    -- ** Request lenses
    gcrsDomainName,

    -- * Destructuring the response
    GetContactReachabilityStatusResponse (..),
    mkGetContactReachabilityStatusResponse,

    -- ** Response lenses
    gcrsrsStatus,
    gcrsrsDomainName,
    gcrsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | /See:/ 'mkGetContactReachabilityStatus' smart constructor.
newtype GetContactReachabilityStatus = GetContactReachabilityStatus'
  { domainName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContactReachabilityStatus' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain for which you want to know whether the registrant contact has confirmed that the email address is valid.
mkGetContactReachabilityStatus ::
  GetContactReachabilityStatus
mkGetContactReachabilityStatus =
  GetContactReachabilityStatus' {domainName = Lude.Nothing}

-- | The name of the domain for which you want to know whether the registrant contact has confirmed that the email address is valid.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsDomainName :: Lens.Lens' GetContactReachabilityStatus (Lude.Maybe Lude.Text)
gcrsDomainName = Lens.lens (domainName :: GetContactReachabilityStatus -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: GetContactReachabilityStatus)
{-# DEPRECATED gcrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest GetContactReachabilityStatus where
  type
    Rs GetContactReachabilityStatus =
      GetContactReachabilityStatusResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContactReachabilityStatusResponse'
            Lude.<$> (x Lude..?> "status")
            Lude.<*> (x Lude..?> "domainName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContactReachabilityStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.GetContactReachabilityStatus" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContactReachabilityStatus where
  toJSON GetContactReachabilityStatus' {..} =
    Lude.object
      (Lude.catMaybes [("domainName" Lude..=) Lude.<$> domainName])

instance Lude.ToPath GetContactReachabilityStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContactReachabilityStatus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContactReachabilityStatusResponse' smart constructor.
data GetContactReachabilityStatusResponse = GetContactReachabilityStatusResponse'
  { status ::
      Lude.Maybe
        ReachabilityStatus,
    domainName ::
      Lude.Maybe
        Lude.Text,
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

-- | Creates a value of 'GetContactReachabilityStatusResponse' with the minimum fields required to make a request.
--
-- * 'domainName' - The domain name for which you requested the reachability status.
-- * 'responseStatus' - The response status code.
-- * 'status' - Whether the registrant contact has responded. Values include the following:
--
--
--     * PENDING
--
--     * We sent the confirmation email and haven't received a response yet.
--
--
--     * DONE
--
--     * We sent the email and got confirmation from the registrant contact.
--
--
--     * EXPIRED
--
--     * The time limit expired before the registrant contact responded.
mkGetContactReachabilityStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContactReachabilityStatusResponse
mkGetContactReachabilityStatusResponse pResponseStatus_ =
  GetContactReachabilityStatusResponse'
    { status = Lude.Nothing,
      domainName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Whether the registrant contact has responded. Values include the following:
--
--
--     * PENDING
--
--     * We sent the confirmation email and haven't received a response yet.
--
--
--     * DONE
--
--     * We sent the email and got confirmation from the registrant contact.
--
--
--     * EXPIRED
--
--     * The time limit expired before the registrant contact responded.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsrsStatus :: Lens.Lens' GetContactReachabilityStatusResponse (Lude.Maybe ReachabilityStatus)
gcrsrsStatus = Lens.lens (status :: GetContactReachabilityStatusResponse -> Lude.Maybe ReachabilityStatus) (\s a -> s {status = a} :: GetContactReachabilityStatusResponse)
{-# DEPRECATED gcrsrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The domain name for which you requested the reachability status.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsrsDomainName :: Lens.Lens' GetContactReachabilityStatusResponse (Lude.Maybe Lude.Text)
gcrsrsDomainName = Lens.lens (domainName :: GetContactReachabilityStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: GetContactReachabilityStatusResponse)
{-# DEPRECATED gcrsrsDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrsrsResponseStatus :: Lens.Lens' GetContactReachabilityStatusResponse Lude.Int
gcrsrsResponseStatus = Lens.lens (responseStatus :: GetContactReachabilityStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContactReachabilityStatusResponse)
{-# DEPRECATED gcrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
