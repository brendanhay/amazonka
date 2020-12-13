{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityMailFromDomainAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the custom MAIL FROM attributes for a list of identities (email addresses : domains).
--
-- This operation is throttled at one request per second and can only get custom MAIL FROM attributes for up to 100 identities at a time.
module Network.AWS.SES.GetIdentityMailFromDomainAttributes
  ( -- * Creating a request
    GetIdentityMailFromDomainAttributes (..),
    mkGetIdentityMailFromDomainAttributes,

    -- ** Request lenses
    gimfdaIdentities,

    -- * Destructuring the response
    GetIdentityMailFromDomainAttributesResponse (..),
    mkGetIdentityMailFromDomainAttributesResponse,

    -- ** Response lenses
    gimfdarsMailFromDomainAttributes,
    gimfdarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to return the Amazon SES custom MAIL FROM attributes for a list of identities. For information about using a custom MAIL FROM domain, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/mail-from.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkGetIdentityMailFromDomainAttributes' smart constructor.
newtype GetIdentityMailFromDomainAttributes = GetIdentityMailFromDomainAttributes'
  { -- | A list of one or more identities.
    identities :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityMailFromDomainAttributes' with the minimum fields required to make a request.
--
-- * 'identities' - A list of one or more identities.
mkGetIdentityMailFromDomainAttributes ::
  GetIdentityMailFromDomainAttributes
mkGetIdentityMailFromDomainAttributes =
  GetIdentityMailFromDomainAttributes' {identities = Lude.mempty}

-- | A list of one or more identities.
--
-- /Note:/ Consider using 'identities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gimfdaIdentities :: Lens.Lens' GetIdentityMailFromDomainAttributes [Lude.Text]
gimfdaIdentities = Lens.lens (identities :: GetIdentityMailFromDomainAttributes -> [Lude.Text]) (\s a -> s {identities = a} :: GetIdentityMailFromDomainAttributes)
{-# DEPRECATED gimfdaIdentities "Use generic-lens or generic-optics with 'identities' instead." #-}

instance Lude.AWSRequest GetIdentityMailFromDomainAttributes where
  type
    Rs GetIdentityMailFromDomainAttributes =
      GetIdentityMailFromDomainAttributesResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "GetIdentityMailFromDomainAttributesResult"
      ( \s h x ->
          GetIdentityMailFromDomainAttributesResponse'
            Lude.<$> ( x Lude..@? "MailFromDomainAttributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLMap "entry" "key" "value"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIdentityMailFromDomainAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetIdentityMailFromDomainAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetIdentityMailFromDomainAttributes where
  toQuery GetIdentityMailFromDomainAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetIdentityMailFromDomainAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Identities" Lude.=: Lude.toQueryList "member" identities
      ]

-- | Represents the custom MAIL FROM attributes for a list of identities.
--
-- /See:/ 'mkGetIdentityMailFromDomainAttributesResponse' smart constructor.
data GetIdentityMailFromDomainAttributesResponse = GetIdentityMailFromDomainAttributesResponse'
  { -- | A map of identities to custom MAIL FROM attributes.
    mailFromDomainAttributes :: Lude.HashMap Lude.Text (IdentityMailFromDomainAttributes),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityMailFromDomainAttributesResponse' with the minimum fields required to make a request.
--
-- * 'mailFromDomainAttributes' - A map of identities to custom MAIL FROM attributes.
-- * 'responseStatus' - The response status code.
mkGetIdentityMailFromDomainAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIdentityMailFromDomainAttributesResponse
mkGetIdentityMailFromDomainAttributesResponse pResponseStatus_ =
  GetIdentityMailFromDomainAttributesResponse'
    { mailFromDomainAttributes =
        Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A map of identities to custom MAIL FROM attributes.
--
-- /Note:/ Consider using 'mailFromDomainAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gimfdarsMailFromDomainAttributes :: Lens.Lens' GetIdentityMailFromDomainAttributesResponse (Lude.HashMap Lude.Text (IdentityMailFromDomainAttributes))
gimfdarsMailFromDomainAttributes = Lens.lens (mailFromDomainAttributes :: GetIdentityMailFromDomainAttributesResponse -> Lude.HashMap Lude.Text (IdentityMailFromDomainAttributes)) (\s a -> s {mailFromDomainAttributes = a} :: GetIdentityMailFromDomainAttributesResponse)
{-# DEPRECATED gimfdarsMailFromDomainAttributes "Use generic-lens or generic-optics with 'mailFromDomainAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gimfdarsResponseStatus :: Lens.Lens' GetIdentityMailFromDomainAttributesResponse Lude.Int
gimfdarsResponseStatus = Lens.lens (responseStatus :: GetIdentityMailFromDomainAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIdentityMailFromDomainAttributesResponse)
{-# DEPRECATED gimfdarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
