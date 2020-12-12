{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPAction
  ( HTTPAction (..),

    -- * Smart constructor
    mkHTTPAction,

    -- * Lenses
    httpaConfirmationURL,
    httpaAuth,
    httpaHeaders,
    httpaUrl,
  )
where

import Network.AWS.IoT.Types.HTTPActionHeader
import Network.AWS.IoT.Types.HTTPAuthorization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Send data to an HTTPS endpoint.
--
-- /See:/ 'mkHTTPAction' smart constructor.
data HTTPAction = HTTPAction'
  { confirmationURL ::
      Lude.Maybe Lude.Text,
    auth :: Lude.Maybe HTTPAuthorization,
    headers :: Lude.Maybe [HTTPActionHeader],
    url :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HTTPAction' with the minimum fields required to make a request.
--
-- * 'auth' - The authentication method to use when sending data to an HTTPS endpoint.
-- * 'confirmationURL' - The URL to which AWS IoT sends a confirmation message. The value of the confirmation URL must be a prefix of the endpoint URL. If you do not specify a confirmation URL AWS IoT uses the endpoint URL as the confirmation URL. If you use substitution templates in the confirmationUrl, you must create and enable topic rule destinations that match each possible value of the substitution template before traffic is allowed to your endpoint URL.
-- * 'headers' - The HTTP headers to send with the message data.
-- * 'url' - The endpoint URL. If substitution templates are used in the URL, you must also specify a @confirmationUrl@ . If this is a new destination, a new @TopicRuleDestination@ is created if possible.
mkHTTPAction ::
  -- | 'url'
  Lude.Text ->
  HTTPAction
mkHTTPAction pUrl_ =
  HTTPAction'
    { confirmationURL = Lude.Nothing,
      auth = Lude.Nothing,
      headers = Lude.Nothing,
      url = pUrl_
    }

-- | The URL to which AWS IoT sends a confirmation message. The value of the confirmation URL must be a prefix of the endpoint URL. If you do not specify a confirmation URL AWS IoT uses the endpoint URL as the confirmation URL. If you use substitution templates in the confirmationUrl, you must create and enable topic rule destinations that match each possible value of the substitution template before traffic is allowed to your endpoint URL.
--
-- /Note:/ Consider using 'confirmationURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpaConfirmationURL :: Lens.Lens' HTTPAction (Lude.Maybe Lude.Text)
httpaConfirmationURL = Lens.lens (confirmationURL :: HTTPAction -> Lude.Maybe Lude.Text) (\s a -> s {confirmationURL = a} :: HTTPAction)
{-# DEPRECATED httpaConfirmationURL "Use generic-lens or generic-optics with 'confirmationURL' instead." #-}

-- | The authentication method to use when sending data to an HTTPS endpoint.
--
-- /Note:/ Consider using 'auth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpaAuth :: Lens.Lens' HTTPAction (Lude.Maybe HTTPAuthorization)
httpaAuth = Lens.lens (auth :: HTTPAction -> Lude.Maybe HTTPAuthorization) (\s a -> s {auth = a} :: HTTPAction)
{-# DEPRECATED httpaAuth "Use generic-lens or generic-optics with 'auth' instead." #-}

-- | The HTTP headers to send with the message data.
--
-- /Note:/ Consider using 'headers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpaHeaders :: Lens.Lens' HTTPAction (Lude.Maybe [HTTPActionHeader])
httpaHeaders = Lens.lens (headers :: HTTPAction -> Lude.Maybe [HTTPActionHeader]) (\s a -> s {headers = a} :: HTTPAction)
{-# DEPRECATED httpaHeaders "Use generic-lens or generic-optics with 'headers' instead." #-}

-- | The endpoint URL. If substitution templates are used in the URL, you must also specify a @confirmationUrl@ . If this is a new destination, a new @TopicRuleDestination@ is created if possible.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
httpaUrl :: Lens.Lens' HTTPAction Lude.Text
httpaUrl = Lens.lens (url :: HTTPAction -> Lude.Text) (\s a -> s {url = a} :: HTTPAction)
{-# DEPRECATED httpaUrl "Use generic-lens or generic-optics with 'url' instead." #-}

instance Lude.FromJSON HTTPAction where
  parseJSON =
    Lude.withObject
      "HTTPAction"
      ( \x ->
          HTTPAction'
            Lude.<$> (x Lude..:? "confirmationUrl")
            Lude.<*> (x Lude..:? "auth")
            Lude.<*> (x Lude..:? "headers" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "url")
      )

instance Lude.ToJSON HTTPAction where
  toJSON HTTPAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("confirmationUrl" Lude..=) Lude.<$> confirmationURL,
            ("auth" Lude..=) Lude.<$> auth,
            ("headers" Lude..=) Lude.<$> headers,
            Lude.Just ("url" Lude..= url)
          ]
      )
