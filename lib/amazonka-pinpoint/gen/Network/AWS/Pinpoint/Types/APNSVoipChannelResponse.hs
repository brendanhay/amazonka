{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSVoipChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSVoipChannelResponse
  ( APNSVoipChannelResponse (..),

    -- * Smart constructor
    mkAPNSVoipChannelResponse,

    -- * Lenses
    avcPlatform,
    avcLastModifiedDate,
    avcEnabled,
    avcHasTokenKey,
    avcDefaultAuthenticationMethod,
    avcIsArchived,
    avcApplicationId,
    avcVersion,
    avcId,
    avcCreationDate,
    avcLastModifiedBy,
    avcHasCredential,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) VoIP channel for an application.
--
-- /See:/ 'mkAPNSVoipChannelResponse' smart constructor.
data APNSVoipChannelResponse = APNSVoipChannelResponse'
  { -- | The type of messaging or notification platform for the channel. For the APNs VoIP channel, this value is APNS_VOIP.
    platform :: Lude.Text,
    -- | The date and time when the APNs VoIP channel was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Text,
    -- | Specifies whether the APNs VoIP channel is enabled for the application.
    enabled :: Lude.Maybe Lude.Bool,
    -- | Specifies whether the APNs VoIP channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
    hasTokenKey :: Lude.Maybe Lude.Bool,
    -- | The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
    defaultAuthenticationMethod :: Lude.Maybe Lude.Text,
    -- | Specifies whether the APNs VoIP channel is archived.
    isArchived :: Lude.Maybe Lude.Bool,
    -- | The unique identifier for the application that the APNs VoIP channel applies to.
    applicationId :: Lude.Maybe Lude.Text,
    -- | The current version of the APNs VoIP channel.
    version :: Lude.Maybe Lude.Int,
    -- | (Deprecated) An identifier for the APNs VoIP channel. This property is retained only for backward compatibility.
    id :: Lude.Maybe Lude.Text,
    -- | The date and time when the APNs VoIP channel was enabled.
    creationDate :: Lude.Maybe Lude.Text,
    -- | The user who last modified the APNs VoIP channel.
    lastModifiedBy :: Lude.Maybe Lude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APNSVoipChannelResponse' with the minimum fields required to make a request.
--
-- * 'platform' - The type of messaging or notification platform for the channel. For the APNs VoIP channel, this value is APNS_VOIP.
-- * 'lastModifiedDate' - The date and time when the APNs VoIP channel was last modified.
-- * 'enabled' - Specifies whether the APNs VoIP channel is enabled for the application.
-- * 'hasTokenKey' - Specifies whether the APNs VoIP channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
-- * 'defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
-- * 'isArchived' - Specifies whether the APNs VoIP channel is archived.
-- * 'applicationId' - The unique identifier for the application that the APNs VoIP channel applies to.
-- * 'version' - The current version of the APNs VoIP channel.
-- * 'id' - (Deprecated) An identifier for the APNs VoIP channel. This property is retained only for backward compatibility.
-- * 'creationDate' - The date and time when the APNs VoIP channel was enabled.
-- * 'lastModifiedBy' - The user who last modified the APNs VoIP channel.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
mkAPNSVoipChannelResponse ::
  -- | 'platform'
  Lude.Text ->
  APNSVoipChannelResponse
mkAPNSVoipChannelResponse pPlatform_ =
  APNSVoipChannelResponse'
    { platform = pPlatform_,
      lastModifiedDate = Lude.Nothing,
      enabled = Lude.Nothing,
      hasTokenKey = Lude.Nothing,
      defaultAuthenticationMethod = Lude.Nothing,
      isArchived = Lude.Nothing,
      applicationId = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      creationDate = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      hasCredential = Lude.Nothing
    }

-- | The type of messaging or notification platform for the channel. For the APNs VoIP channel, this value is APNS_VOIP.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcPlatform :: Lens.Lens' APNSVoipChannelResponse Lude.Text
avcPlatform = Lens.lens (platform :: APNSVoipChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The date and time when the APNs VoIP channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcLastModifiedDate :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Text)
avcLastModifiedDate = Lens.lens (lastModifiedDate :: APNSVoipChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the APNs VoIP channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcEnabled :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Bool)
avcEnabled = Lens.lens (enabled :: APNSVoipChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether the APNs VoIP channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- /Note:/ Consider using 'hasTokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcHasTokenKey :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Bool)
avcHasTokenKey = Lens.lens (hasTokenKey :: APNSVoipChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasTokenKey = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcHasTokenKey "Use generic-lens or generic-optics with 'hasTokenKey' instead." #-}

-- | The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcDefaultAuthenticationMethod :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Text)
avcDefaultAuthenticationMethod = Lens.lens (defaultAuthenticationMethod :: APNSVoipChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultAuthenticationMethod = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | Specifies whether the APNs VoIP channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcIsArchived :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Bool)
avcIsArchived = Lens.lens (isArchived :: APNSVoipChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the APNs VoIP channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcApplicationId :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Text)
avcApplicationId = Lens.lens (applicationId :: APNSVoipChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the APNs VoIP channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcVersion :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Int)
avcVersion = Lens.lens (version :: APNSVoipChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the APNs VoIP channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcId :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Text)
avcId = Lens.lens (id :: APNSVoipChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the APNs VoIP channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcCreationDate :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Text)
avcCreationDate = Lens.lens (creationDate :: APNSVoipChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the APNs VoIP channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcLastModifiedBy :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Text)
avcLastModifiedBy = Lens.lens (lastModifiedBy :: APNSVoipChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avcHasCredential :: Lens.Lens' APNSVoipChannelResponse (Lude.Maybe Lude.Bool)
avcHasCredential = Lens.lens (hasCredential :: APNSVoipChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: APNSVoipChannelResponse)
{-# DEPRECATED avcHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

instance Lude.FromJSON APNSVoipChannelResponse where
  parseJSON =
    Lude.withObject
      "APNSVoipChannelResponse"
      ( \x ->
          APNSVoipChannelResponse'
            Lude.<$> (x Lude..: "Platform")
            Lude.<*> (x Lude..:? "LastModifiedDate")
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
      )
