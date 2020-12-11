-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailChannelRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailChannelRequest
  ( EmailChannelRequest (..),

    -- * Smart constructor
    mkEmailChannelRequest,

    -- * Lenses
    ecrEnabled,
    ecrConfigurationSet,
    ecrRoleARN,
    ecrFromAddress,
    ecrIdentity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the status and settings of the email channel for an application.
--
-- /See:/ 'mkEmailChannelRequest' smart constructor.
data EmailChannelRequest = EmailChannelRequest'
  { enabled ::
      Lude.Maybe Lude.Bool,
    configurationSet :: Lude.Maybe Lude.Text,
    roleARN :: Lude.Maybe Lude.Text,
    fromAddress :: Lude.Text,
    identity :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmailChannelRequest' with the minimum fields required to make a request.
--
-- * 'configurationSet' - The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that you want to apply to messages that you send through the channel.
-- * 'enabled' - Specifies whether to enable the email channel for the application.
-- * 'fromAddress' - The verified email address that you want to send email from when you send email through the channel.
-- * 'identity' - The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that you want to use when you send email through the channel.
-- * 'roleARN' - The ARN of the AWS Identity and Access Management (IAM) role that you want Amazon Pinpoint to use when it submits email-related event data for the channel.
mkEmailChannelRequest ::
  -- | 'fromAddress'
  Lude.Text ->
  -- | 'identity'
  Lude.Text ->
  EmailChannelRequest
mkEmailChannelRequest pFromAddress_ pIdentity_ =
  EmailChannelRequest'
    { enabled = Lude.Nothing,
      configurationSet = Lude.Nothing,
      roleARN = Lude.Nothing,
      fromAddress = pFromAddress_,
      identity = pIdentity_
    }

-- | Specifies whether to enable the email channel for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrEnabled :: Lens.Lens' EmailChannelRequest (Lude.Maybe Lude.Bool)
ecrEnabled = Lens.lens (enabled :: EmailChannelRequest -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: EmailChannelRequest)
{-# DEPRECATED ecrEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The <https://docs.aws.amazon.com/ses/latest/APIReference/API_ConfigurationSet.html Amazon SES configuration set> that you want to apply to messages that you send through the channel.
--
-- /Note:/ Consider using 'configurationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrConfigurationSet :: Lens.Lens' EmailChannelRequest (Lude.Maybe Lude.Text)
ecrConfigurationSet = Lens.lens (configurationSet :: EmailChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {configurationSet = a} :: EmailChannelRequest)
{-# DEPRECATED ecrConfigurationSet "Use generic-lens or generic-optics with 'configurationSet' instead." #-}

-- | The ARN of the AWS Identity and Access Management (IAM) role that you want Amazon Pinpoint to use when it submits email-related event data for the channel.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrRoleARN :: Lens.Lens' EmailChannelRequest (Lude.Maybe Lude.Text)
ecrRoleARN = Lens.lens (roleARN :: EmailChannelRequest -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: EmailChannelRequest)
{-# DEPRECATED ecrRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The verified email address that you want to send email from when you send email through the channel.
--
-- /Note:/ Consider using 'fromAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrFromAddress :: Lens.Lens' EmailChannelRequest Lude.Text
ecrFromAddress = Lens.lens (fromAddress :: EmailChannelRequest -> Lude.Text) (\s a -> s {fromAddress = a} :: EmailChannelRequest)
{-# DEPRECATED ecrFromAddress "Use generic-lens or generic-optics with 'fromAddress' instead." #-}

-- | The Amazon Resource Name (ARN) of the identity, verified with Amazon Simple Email Service (Amazon SES), that you want to use when you send email through the channel.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecrIdentity :: Lens.Lens' EmailChannelRequest Lude.Text
ecrIdentity = Lens.lens (identity :: EmailChannelRequest -> Lude.Text) (\s a -> s {identity = a} :: EmailChannelRequest)
{-# DEPRECATED ecrIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

instance Lude.ToJSON EmailChannelRequest where
  toJSON EmailChannelRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("ConfigurationSet" Lude..=) Lude.<$> configurationSet,
            ("RoleArn" Lude..=) Lude.<$> roleARN,
            Lude.Just ("FromAddress" Lude..= fromAddress),
            Lude.Just ("Identity" Lude..= identity)
          ]
      )
