{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.UpdateDomainContactPrivacy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates the specified domain contact's privacy setting. When privacy protection is enabled, contact information such as email address is replaced either with contact information for Amazon Registrar (for .com, .net, and .org domains) or with contact information for our registrar associate, Gandi.
--
-- This operation affects only the contact information for the specified contact type (registrant, administrator, or tech). If the request succeeds, Amazon Route 53 returns an operation ID that you can use with <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail> to track the progress and completion of the action. If the request doesn't complete successfully, the domain registrant will be notified by email.
-- /Important:/ By disabling the privacy service via API, you consent to the publication of the contact information provided for this domain via the public WHOIS database. You certify that you are the registrant of this domain name and have the authority to make this decision. You may withdraw your consent at any time by enabling privacy protection using either @UpdateDomainContactPrivacy@ or the Route 53 console. Enabling privacy protection removes the contact information provided for this domain from the WHOIS database. For more information on our privacy practices, see <https://aws.amazon.com/privacy/ https://aws.amazon.com/privacy/> .
module Network.AWS.Route53Domains.UpdateDomainContactPrivacy
  ( -- * Creating a request
    UpdateDomainContactPrivacy (..),
    mkUpdateDomainContactPrivacy,

    -- ** Request lenses
    udcpTechPrivacy,
    udcpRegistrantPrivacy,
    udcpAdminPrivacy,
    udcpDomainName,

    -- * Destructuring the response
    UpdateDomainContactPrivacyResponse (..),
    mkUpdateDomainContactPrivacyResponse,

    -- ** Response lenses
    udcprsResponseStatus,
    udcprsOperationId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | The UpdateDomainContactPrivacy request includes the following elements.
--
-- /See:/ 'mkUpdateDomainContactPrivacy' smart constructor.
data UpdateDomainContactPrivacy = UpdateDomainContactPrivacy'
  { techPrivacy ::
      Lude.Maybe Lude.Bool,
    registrantPrivacy ::
      Lude.Maybe Lude.Bool,
    adminPrivacy :: Lude.Maybe Lude.Bool,
    domainName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainContactPrivacy' with the minimum fields required to make a request.
--
-- * 'adminPrivacy' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
-- * 'domainName' - The name of the domain that you want to update the privacy setting for.
-- * 'registrantPrivacy' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
-- * 'techPrivacy' - Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
mkUpdateDomainContactPrivacy ::
  -- | 'domainName'
  Lude.Text ->
  UpdateDomainContactPrivacy
mkUpdateDomainContactPrivacy pDomainName_ =
  UpdateDomainContactPrivacy'
    { techPrivacy = Lude.Nothing,
      registrantPrivacy = Lude.Nothing,
      adminPrivacy = Lude.Nothing,
      domainName = pDomainName_
    }

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the technical contact.
--
-- /Note:/ Consider using 'techPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcpTechPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Lude.Maybe Lude.Bool)
udcpTechPrivacy = Lens.lens (techPrivacy :: UpdateDomainContactPrivacy -> Lude.Maybe Lude.Bool) (\s a -> s {techPrivacy = a} :: UpdateDomainContactPrivacy)
{-# DEPRECATED udcpTechPrivacy "Use generic-lens or generic-optics with 'techPrivacy' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the registrant contact (domain owner).
--
-- /Note:/ Consider using 'registrantPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcpRegistrantPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Lude.Maybe Lude.Bool)
udcpRegistrantPrivacy = Lens.lens (registrantPrivacy :: UpdateDomainContactPrivacy -> Lude.Maybe Lude.Bool) (\s a -> s {registrantPrivacy = a} :: UpdateDomainContactPrivacy)
{-# DEPRECATED udcpRegistrantPrivacy "Use generic-lens or generic-optics with 'registrantPrivacy' instead." #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you specify @true@ , WHOIS ("who is") queries return contact information either for Amazon Registrar (for .com, .net, and .org domains) or for our registrar associate, Gandi (for all other TLDs). If you specify @false@ , WHOIS queries return the information that you entered for the admin contact.
--
-- /Note:/ Consider using 'adminPrivacy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcpAdminPrivacy :: Lens.Lens' UpdateDomainContactPrivacy (Lude.Maybe Lude.Bool)
udcpAdminPrivacy = Lens.lens (adminPrivacy :: UpdateDomainContactPrivacy -> Lude.Maybe Lude.Bool) (\s a -> s {adminPrivacy = a} :: UpdateDomainContactPrivacy)
{-# DEPRECATED udcpAdminPrivacy "Use generic-lens or generic-optics with 'adminPrivacy' instead." #-}

-- | The name of the domain that you want to update the privacy setting for.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcpDomainName :: Lens.Lens' UpdateDomainContactPrivacy Lude.Text
udcpDomainName = Lens.lens (domainName :: UpdateDomainContactPrivacy -> Lude.Text) (\s a -> s {domainName = a} :: UpdateDomainContactPrivacy)
{-# DEPRECATED udcpDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest UpdateDomainContactPrivacy where
  type
    Rs UpdateDomainContactPrivacy =
      UpdateDomainContactPrivacyResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDomainContactPrivacyResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "OperationId")
      )

instance Lude.ToHeaders UpdateDomainContactPrivacy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.UpdateDomainContactPrivacy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDomainContactPrivacy where
  toJSON UpdateDomainContactPrivacy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TechPrivacy" Lude..=) Lude.<$> techPrivacy,
            ("RegistrantPrivacy" Lude..=) Lude.<$> registrantPrivacy,
            ("AdminPrivacy" Lude..=) Lude.<$> adminPrivacy,
            Lude.Just ("DomainName" Lude..= domainName)
          ]
      )

instance Lude.ToPath UpdateDomainContactPrivacy where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDomainContactPrivacy where
  toQuery = Lude.const Lude.mempty

-- | The UpdateDomainContactPrivacy response includes the following element.
--
-- /See:/ 'mkUpdateDomainContactPrivacyResponse' smart constructor.
data UpdateDomainContactPrivacyResponse = UpdateDomainContactPrivacyResponse'
  { responseStatus ::
      Lude.Int,
    operationId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDomainContactPrivacyResponse' with the minimum fields required to make a request.
--
-- * 'operationId' - Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
-- * 'responseStatus' - The response status code.
mkUpdateDomainContactPrivacyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'operationId'
  Lude.Text ->
  UpdateDomainContactPrivacyResponse
mkUpdateDomainContactPrivacyResponse pResponseStatus_ pOperationId_ =
  UpdateDomainContactPrivacyResponse'
    { responseStatus =
        pResponseStatus_,
      operationId = pOperationId_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcprsResponseStatus :: Lens.Lens' UpdateDomainContactPrivacyResponse Lude.Int
udcprsResponseStatus = Lens.lens (responseStatus :: UpdateDomainContactPrivacyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDomainContactPrivacyResponse)
{-# DEPRECATED udcprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcprsOperationId :: Lens.Lens' UpdateDomainContactPrivacyResponse Lude.Text
udcprsOperationId = Lens.lens (operationId :: UpdateDomainContactPrivacyResponse -> Lude.Text) (\s a -> s {operationId = a} :: UpdateDomainContactPrivacyResponse)
{-# DEPRECATED udcprsOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}
