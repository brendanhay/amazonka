{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityDkimAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of Easy DKIM signing for an entity. For domain name identities, this operation also returns the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES has successfully verified that these tokens have been published.
--
-- This operation takes a list of identities as input and returns the following information for each:
--
--     * Whether Easy DKIM signing is enabled or disabled.
--
--
--     * A set of DKIM tokens that represent the identity. If the identity is an email address, the tokens represent the domain of that address.
--
--
--     * Whether Amazon SES has successfully verified the DKIM tokens published in the domain's DNS. This information is only returned for domain name identities, not for email addresses.
--
--
-- This operation is throttled at one request per second and can only get DKIM attributes for up to 100 identities at a time.
-- For more information about creating DNS records using DKIM tokens, go to the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim-dns-records.html Amazon SES Developer Guide> .
module Network.AWS.SES.GetIdentityDkimAttributes
  ( -- * Creating a request
    GetIdentityDkimAttributes (..),
    mkGetIdentityDkimAttributes,

    -- ** Request lenses
    gidaIdentities,

    -- * Destructuring the response
    GetIdentityDkimAttributesResponse (..),
    mkGetIdentityDkimAttributesResponse,

    -- ** Response lenses
    gidarsDkimAttributes,
    gidarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request for the status of Amazon SES Easy DKIM signing for an identity. For domain identities, this request also returns the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES successfully verified that these tokens were published. For more information about Easy DKIM, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkGetIdentityDkimAttributes' smart constructor.
newtype GetIdentityDkimAttributes = GetIdentityDkimAttributes'
  { -- | A list of one or more verified identities - email addresses, domains, or both.
    identities :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityDkimAttributes' with the minimum fields required to make a request.
--
-- * 'identities' - A list of one or more verified identities - email addresses, domains, or both.
mkGetIdentityDkimAttributes ::
  GetIdentityDkimAttributes
mkGetIdentityDkimAttributes =
  GetIdentityDkimAttributes' {identities = Lude.mempty}

-- | A list of one or more verified identities - email addresses, domains, or both.
--
-- /Note:/ Consider using 'identities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gidaIdentities :: Lens.Lens' GetIdentityDkimAttributes [Lude.Text]
gidaIdentities = Lens.lens (identities :: GetIdentityDkimAttributes -> [Lude.Text]) (\s a -> s {identities = a} :: GetIdentityDkimAttributes)
{-# DEPRECATED gidaIdentities "Use generic-lens or generic-optics with 'identities' instead." #-}

instance Lude.AWSRequest GetIdentityDkimAttributes where
  type
    Rs GetIdentityDkimAttributes =
      GetIdentityDkimAttributesResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "GetIdentityDkimAttributesResult"
      ( \s h x ->
          GetIdentityDkimAttributesResponse'
            Lude.<$> ( x Lude..@? "DkimAttributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLMap "entry" "key" "value"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIdentityDkimAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetIdentityDkimAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetIdentityDkimAttributes where
  toQuery GetIdentityDkimAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetIdentityDkimAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Identities" Lude.=: Lude.toQueryList "member" identities
      ]

-- | Represents the status of Amazon SES Easy DKIM signing for an identity. For domain identities, this response also contains the DKIM tokens that are required for Easy DKIM signing, and whether Amazon SES successfully verified that these tokens were published.
--
-- /See:/ 'mkGetIdentityDkimAttributesResponse' smart constructor.
data GetIdentityDkimAttributesResponse = GetIdentityDkimAttributesResponse'
  { -- | The DKIM attributes for an email address or a domain.
    dkimAttributes :: Lude.HashMap Lude.Text (IdentityDkimAttributes),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityDkimAttributesResponse' with the minimum fields required to make a request.
--
-- * 'dkimAttributes' - The DKIM attributes for an email address or a domain.
-- * 'responseStatus' - The response status code.
mkGetIdentityDkimAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIdentityDkimAttributesResponse
mkGetIdentityDkimAttributesResponse pResponseStatus_ =
  GetIdentityDkimAttributesResponse'
    { dkimAttributes = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The DKIM attributes for an email address or a domain.
--
-- /Note:/ Consider using 'dkimAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gidarsDkimAttributes :: Lens.Lens' GetIdentityDkimAttributesResponse (Lude.HashMap Lude.Text (IdentityDkimAttributes))
gidarsDkimAttributes = Lens.lens (dkimAttributes :: GetIdentityDkimAttributesResponse -> Lude.HashMap Lude.Text (IdentityDkimAttributes)) (\s a -> s {dkimAttributes = a} :: GetIdentityDkimAttributesResponse)
{-# DEPRECATED gidarsDkimAttributes "Use generic-lens or generic-optics with 'dkimAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gidarsResponseStatus :: Lens.Lens' GetIdentityDkimAttributesResponse Lude.Int
gidarsResponseStatus = Lens.lens (responseStatus :: GetIdentityDkimAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIdentityDkimAttributesResponse)
{-# DEPRECATED gidarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
