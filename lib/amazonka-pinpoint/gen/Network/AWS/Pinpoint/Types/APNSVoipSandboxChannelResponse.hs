{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipSandboxChannelResponse
  ( APNSVoipSandboxChannelResponse (..),

    -- * Smart constructor
    mkAPNSVoipSandboxChannelResponse,

    -- * Lenses
    avscLastModifiedDate,
    avscEnabled,
    avscHasTokenKey,
    avscDefaultAuthenticationMethod,
    avscIsArchived,
    avscApplicationId,
    avscVersion,
    avscId,
    avscCreationDate,
    avscLastModifiedBy,
    avscHasCredential,
    avscPlatform,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) VoIP sandbox channel for an application.
--
-- /See:/ 'mkAPNSVoipSandboxChannelResponse' smart constructor.
data APNSVoipSandboxChannelResponse = APNSVoipSandboxChannelResponse'
  { lastModifiedDate ::
      Lude.Maybe Lude.Text,
    enabled ::
      Lude.Maybe Lude.Bool,
    hasTokenKey ::
      Lude.Maybe Lude.Bool,
    defaultAuthenticationMethod ::
      Lude.Maybe Lude.Text,
    isArchived ::
      Lude.Maybe Lude.Bool,
    applicationId ::
      Lude.Maybe Lude.Text,
    version ::
      Lude.Maybe Lude.Int,
    id :: Lude.Maybe Lude.Text,
    creationDate ::
      Lude.Maybe Lude.Text,
    lastModifiedBy ::
      Lude.Maybe Lude.Text,
    hasCredential ::
      Lude.Maybe Lude.Bool,
    platform :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APNSVoipSandboxChannelResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the APNs VoIP sandbox channel applies to.
-- * 'creationDate' - The date and time when the APNs VoIP sandbox channel was enabled.
-- * 'defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
-- * 'enabled' - Specifies whether the APNs VoIP sandbox channel is enabled for the application.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
-- * 'hasTokenKey' - Specifies whether the APNs VoIP sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
-- * 'id' - (Deprecated) An identifier for the APNs VoIP sandbox channel. This property is retained only for backward compatibility.
-- * 'isArchived' - Specifies whether the APNs VoIP sandbox channel is archived.
-- * 'lastModifiedBy' - The user who last modified the APNs VoIP sandbox channel.
-- * 'lastModifiedDate' - The date and time when the APNs VoIP sandbox channel was last modified.
-- * 'platform' - The type of messaging or notification platform for the channel. For the APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
-- * 'version' - The current version of the APNs VoIP sandbox channel.
mkAPNSVoipSandboxChannelResponse ::
  -- | 'platform'
  Lude.Text ->
  APNSVoipSandboxChannelResponse
mkAPNSVoipSandboxChannelResponse pPlatform_ =
  APNSVoipSandboxChannelResponse'
    { lastModifiedDate = Lude.Nothing,
      enabled = Lude.Nothing,
      hasTokenKey = Lude.Nothing,
      defaultAuthenticationMethod = Lude.Nothing,
      isArchived = Lude.Nothing,
      applicationId = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      creationDate = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      hasCredential = Lude.Nothing,
      platform = pPlatform_
    }

-- | The date and time when the APNs VoIP sandbox channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscLastModifiedDate :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Text)
avscLastModifiedDate = Lens.lens (lastModifiedDate :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the APNs VoIP sandbox channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscEnabled :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Bool)
avscEnabled = Lens.lens (enabled :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether the APNs VoIP sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- /Note:/ Consider using 'hasTokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscHasTokenKey :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Bool)
avscHasTokenKey = Lens.lens (hasTokenKey :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasTokenKey = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscHasTokenKey "Use generic-lens or generic-optics with 'hasTokenKey' instead." #-}

-- | The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscDefaultAuthenticationMethod :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Text)
avscDefaultAuthenticationMethod = Lens.lens (defaultAuthenticationMethod :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultAuthenticationMethod = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | Specifies whether the APNs VoIP sandbox channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscIsArchived :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Bool)
avscIsArchived = Lens.lens (isArchived :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the APNs VoIP sandbox channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscApplicationId :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Text)
avscApplicationId = Lens.lens (applicationId :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the APNs VoIP sandbox channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscVersion :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Int)
avscVersion = Lens.lens (version :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the APNs VoIP sandbox channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscId :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Text)
avscId = Lens.lens (id :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the APNs VoIP sandbox channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscCreationDate :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Text)
avscCreationDate = Lens.lens (creationDate :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the APNs VoIP sandbox channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscLastModifiedBy :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Text)
avscLastModifiedBy = Lens.lens (lastModifiedBy :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscHasCredential :: Lens.Lens' APNSVoipSandboxChannelResponse (Lude.Maybe Lude.Bool)
avscHasCredential = Lens.lens (hasCredential :: APNSVoipSandboxChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | The type of messaging or notification platform for the channel. For the APNs VoIP sandbox channel, this value is APNS_VOIP_SANDBOX.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avscPlatform :: Lens.Lens' APNSVoipSandboxChannelResponse Lude.Text
avscPlatform = Lens.lens (platform :: APNSVoipSandboxChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: APNSVoipSandboxChannelResponse)
{-# DEPRECATED avscPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

instance Lude.FromJSON APNSVoipSandboxChannelResponse where
  parseJSON =
    Lude.withObject
      "APNSVoipSandboxChannelResponse"
      ( \x ->
          APNSVoipSandboxChannelResponse'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "HasTokenKey")
            Lude.<*> (x Lude..:? "DefaultAuthenticationMethod")
            Lude.<*> (x Lude..:? "IsArchived")
            Lude.<*> (x Lude..:? "ApplicationId")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "HasCredential")
            Lude.<*> (x Lude..: "Platform")
      )
