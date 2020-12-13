{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateDomainNameservers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation replaces the current set of name servers for the domain with the specified set of name servers. If you use Amazon Route 53 as your DNS service, specify the four name servers in the delegation set for the hosted zone for the domain.
--
-- If successful, this operation returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
module Network.AWS.Route53Domains.UpdateDomainNameservers
  ( -- * Creating a request
    UpdateDomainNameservers (..),
    mkUpdateDomainNameservers,

    -- ** Request lenses
    udnDomainName,
    udnFIAuthKey,
    udnNameservers,

    -- * Destructuring the response
    UpdateDomainNameserversResponse (..),
    mkUpdateDomainNameserversResponse,

    -- ** Response lenses
    udnrsOperationId,
    udnrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | Replaces the current set of name servers for the domain with the specified set of name servers. If you use Amazon Route 53 as your DNS service, specify the four name servers in the delegation set for the hosted zone for the domain.
--
-- If successful, this operation returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant will be notified by email.
--
-- /See:/ 'mkUpdateDomainNameservers' smart constructor.
data UpdateDomainNameservers = UpdateDomainNameservers'
  { -- | The name of the domain that you want to change name servers for.
    domainName :: Lude.Text,
    -- | The authorization key for .fi domains
    fIAuthKey :: Lude.Maybe Lude.Text,
    -- | A list of new name servers for the domain.
    nameservers :: [Nameserver]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainNameservers' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain that you want to change name servers for.
-- * 'fIAuthKey' - The authorization key for .fi domains
-- * 'nameservers' - A list of new name servers for the domain.
mkUpdateDomainNameservers ::
  -- | 'domainName'
  Lude.Text ->
  UpdateDomainNameservers
mkUpdateDomainNameservers pDomainName_ =
  UpdateDomainNameservers'
    { domainName = pDomainName_,
      fIAuthKey = Lude.Nothing,
      nameservers = Lude.mempty
    }

-- | The name of the domain that you want to change name servers for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnDomainName :: Lens.Lens' UpdateDomainNameservers Lude.Text
udnDomainName = Lens.lens (domainName :: UpdateDomainNameservers -> Lude.Text) (\s a -> s {domainName = a} :: UpdateDomainNameservers)
{-# DEPRECATED udnDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The authorization key for .fi domains
--
-- /Note:/ Consider using 'fIAuthKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnFIAuthKey :: Lens.Lens' UpdateDomainNameservers (Lude.Maybe Lude.Text)
udnFIAuthKey = Lens.lens (fIAuthKey :: UpdateDomainNameservers -> Lude.Maybe Lude.Text) (\s a -> s {fIAuthKey = a} :: UpdateDomainNameservers)
{-# DEPRECATED udnFIAuthKey "Use generic-lens or generic-optics with 'fIAuthKey' instead." #-}

-- | A list of new name servers for the domain.
--
-- /Note:/ Consider using 'nameservers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnNameservers :: Lens.Lens' UpdateDomainNameservers [Nameserver]
udnNameservers = Lens.lens (nameservers :: UpdateDomainNameservers -> [Nameserver]) (\s a -> s {nameservers = a} :: UpdateDomainNameservers)
{-# DEPRECATED udnNameservers "Use generic-lens or generic-optics with 'nameservers' instead." #-}

instance Lude.AWSRequest UpdateDomainNameservers where
  type Rs UpdateDomainNameservers = UpdateDomainNameserversResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDomainNameserversResponse'
            Lude.<$> (x Lude..:> "OperationId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDomainNameservers where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.UpdateDomainNameservers" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDomainNameservers where
  toJSON UpdateDomainNameservers' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DomainName" Lude..= domainName),
            ("FIAuthKey" Lude..=) Lude.<$> fIAuthKey,
            Lude.Just ("Nameservers" Lude..= nameservers)
          ]
      )

instance Lude.ToPath UpdateDomainNameservers where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDomainNameservers where
  toQuery = Lude.const Lude.mempty

-- | The UpdateDomainNameservers response includes the following element.
--
-- /See:/ 'mkUpdateDomainNameserversResponse' smart constructor.
data UpdateDomainNameserversResponse = UpdateDomainNameserversResponse'
  { -- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
    operationId :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainNameserversResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
-- * 'responseStatus' - The response status code.
mkUpdateDomainNameserversResponse ::
  -- | 'operationId'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDomainNameserversResponse
mkUpdateDomainNameserversResponse pOperationId_ pResponseStatus_ =
  UpdateDomainNameserversResponse'
    { operationId = pOperationId_,
      responseStatus = pResponseStatus_
    }

-- | Identifier for tracking the progress of the request. To query the operation status, use <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> .
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnrsOperationId :: Lens.Lens' UpdateDomainNameserversResponse Lude.Text
udnrsOperationId = Lens.lens (operationId :: UpdateDomainNameserversResponse -> Lude.Text) (\s a -> s {operationId = a} :: UpdateDomainNameserversResponse)
{-# DEPRECATED udnrsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udnrsResponseStatus :: Lens.Lens' UpdateDomainNameserversResponse Lude.Int
udnrsResponseStatus = Lens.lens (responseStatus :: UpdateDomainNameserversResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDomainNameserversResponse)
{-# DEPRECATED udnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
