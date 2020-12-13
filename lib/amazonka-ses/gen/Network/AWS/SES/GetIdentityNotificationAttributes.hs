{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityNotificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a list of verified identities (email addresses and/or domains), returns a structure describing identity notification attributes.
--
-- This operation is throttled at one request per second and can only get notification attributes for up to 100 identities at a time.
-- For more information about using notifications with Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide> .
module Network.AWS.SES.GetIdentityNotificationAttributes
  ( -- * Creating a request
    GetIdentityNotificationAttributes (..),
    mkGetIdentityNotificationAttributes,

    -- ** Request lenses
    ginaIdentities,

    -- * Destructuring the response
    GetIdentityNotificationAttributesResponse (..),
    mkGetIdentityNotificationAttributesResponse,

    -- ** Response lenses
    ginarsNotificationAttributes,
    ginarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to return the notification attributes for a list of identities you verified with Amazon SES. For information about Amazon SES notifications, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkGetIdentityNotificationAttributes' smart constructor.
newtype GetIdentityNotificationAttributes = GetIdentityNotificationAttributes'
  { -- | A list of one or more identities. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
    identities :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityNotificationAttributes' with the minimum fields required to make a request.
--
-- * 'identities' - A list of one or more identities. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
mkGetIdentityNotificationAttributes ::
  GetIdentityNotificationAttributes
mkGetIdentityNotificationAttributes =
  GetIdentityNotificationAttributes' {identities = Lude.mempty}

-- | A list of one or more identities. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- /Note:/ Consider using 'identities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ginaIdentities :: Lens.Lens' GetIdentityNotificationAttributes [Lude.Text]
ginaIdentities = Lens.lens (identities :: GetIdentityNotificationAttributes -> [Lude.Text]) (\s a -> s {identities = a} :: GetIdentityNotificationAttributes)
{-# DEPRECATED ginaIdentities "Use generic-lens or generic-optics with 'identities' instead." #-}

instance Lude.AWSRequest GetIdentityNotificationAttributes where
  type
    Rs GetIdentityNotificationAttributes =
      GetIdentityNotificationAttributesResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "GetIdentityNotificationAttributesResult"
      ( \s h x ->
          GetIdentityNotificationAttributesResponse'
            Lude.<$> ( x Lude..@? "NotificationAttributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLMap "entry" "key" "value"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIdentityNotificationAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetIdentityNotificationAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetIdentityNotificationAttributes where
  toQuery GetIdentityNotificationAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetIdentityNotificationAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Identities" Lude.=: Lude.toQueryList "member" identities
      ]

-- | Represents the notification attributes for a list of identities.
--
-- /See:/ 'mkGetIdentityNotificationAttributesResponse' smart constructor.
data GetIdentityNotificationAttributesResponse = GetIdentityNotificationAttributesResponse'
  { -- | A map of Identity to IdentityNotificationAttributes.
    notificationAttributes :: Lude.HashMap Lude.Text (IdentityNotificationAttributes),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityNotificationAttributesResponse' with the minimum fields required to make a request.
--
-- * 'notificationAttributes' - A map of Identity to IdentityNotificationAttributes.
-- * 'responseStatus' - The response status code.
mkGetIdentityNotificationAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIdentityNotificationAttributesResponse
mkGetIdentityNotificationAttributesResponse pResponseStatus_ =
  GetIdentityNotificationAttributesResponse'
    { notificationAttributes =
        Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A map of Identity to IdentityNotificationAttributes.
--
-- /Note:/ Consider using 'notificationAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ginarsNotificationAttributes :: Lens.Lens' GetIdentityNotificationAttributesResponse (Lude.HashMap Lude.Text (IdentityNotificationAttributes))
ginarsNotificationAttributes = Lens.lens (notificationAttributes :: GetIdentityNotificationAttributesResponse -> Lude.HashMap Lude.Text (IdentityNotificationAttributes)) (\s a -> s {notificationAttributes = a} :: GetIdentityNotificationAttributesResponse)
{-# DEPRECATED ginarsNotificationAttributes "Use generic-lens or generic-optics with 'notificationAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ginarsResponseStatus :: Lens.Lens' GetIdentityNotificationAttributesResponse Lude.Int
ginarsResponseStatus = Lens.lens (responseStatus :: GetIdentityNotificationAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIdentityNotificationAttributesResponse)
{-# DEPRECATED ginarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
