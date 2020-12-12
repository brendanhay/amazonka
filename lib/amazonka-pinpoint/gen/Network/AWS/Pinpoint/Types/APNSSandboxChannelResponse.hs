{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSSandboxChannelResponse
  ( APNSSandboxChannelResponse (..),

    -- * Smart constructor
    mkAPNSSandboxChannelResponse,

    -- * Lenses
    ascLastModifiedDate,
    ascEnabled,
    ascHasTokenKey,
    ascDefaultAuthenticationMethod,
    ascIsArchived,
    ascApplicationId,
    ascVersion,
    ascId,
    ascCreationDate,
    ascLastModifiedBy,
    ascHasCredential,
    ascPlatform,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) sandbox channel for an application.
--
-- /See:/ 'mkAPNSSandboxChannelResponse' smart constructor.
data APNSSandboxChannelResponse = APNSSandboxChannelResponse'
  { lastModifiedDate ::
      Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    hasTokenKey :: Lude.Maybe Lude.Bool,
    defaultAuthenticationMethod ::
      Lude.Maybe Lude.Text,
    isArchived :: Lude.Maybe Lude.Bool,
    applicationId :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Int,
    id :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Text,
    lastModifiedBy ::
      Lude.Maybe Lude.Text,
    hasCredential :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'APNSSandboxChannelResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the APNs sandbox channel applies to.
-- * 'creationDate' - The date and time when the APNs sandbox channel was enabled.
-- * 'defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
-- * 'enabled' - Specifies whether the APNs sandbox channel is enabled for the application.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
-- * 'hasTokenKey' - Specifies whether the APNs sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
-- * 'id' - (Deprecated) An identifier for the APNs sandbox channel. This property is retained only for backward compatibility.
-- * 'isArchived' - Specifies whether the APNs sandbox channel is archived.
-- * 'lastModifiedBy' - The user who last modified the APNs sandbox channel.
-- * 'lastModifiedDate' - The date and time when the APNs sandbox channel was last modified.
-- * 'platform' - The type of messaging or notification platform for the channel. For the APNs sandbox channel, this value is APNS_SANDBOX.
-- * 'version' - The current version of the APNs sandbox channel.
mkAPNSSandboxChannelResponse ::
  -- | 'platform'
  Lude.Text ->
  APNSSandboxChannelResponse
mkAPNSSandboxChannelResponse pPlatform_ =
  APNSSandboxChannelResponse'
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

-- | The date and time when the APNs sandbox channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascLastModifiedDate :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Text)
ascLastModifiedDate = Lens.lens (lastModifiedDate :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the APNs sandbox channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascEnabled :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Bool)
ascEnabled = Lens.lens (enabled :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether the APNs sandbox channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- /Note:/ Consider using 'hasTokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascHasTokenKey :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Bool)
ascHasTokenKey = Lens.lens (hasTokenKey :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasTokenKey = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascHasTokenKey "Use generic-lens or generic-optics with 'hasTokenKey' instead." #-}

-- | The default authentication method that Amazon Pinpoint uses to authenticate with the APNs sandbox environment for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascDefaultAuthenticationMethod :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Text)
ascDefaultAuthenticationMethod = Lens.lens (defaultAuthenticationMethod :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultAuthenticationMethod = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | Specifies whether the APNs sandbox channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascIsArchived :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Bool)
ascIsArchived = Lens.lens (isArchived :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the APNs sandbox channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascApplicationId :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Text)
ascApplicationId = Lens.lens (applicationId :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the APNs sandbox channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascVersion :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Int)
ascVersion = Lens.lens (version :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the APNs sandbox channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascId :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Text)
ascId = Lens.lens (id :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the APNs sandbox channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascCreationDate :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Text)
ascCreationDate = Lens.lens (creationDate :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the APNs sandbox channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascLastModifiedBy :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Text)
ascLastModifiedBy = Lens.lens (lastModifiedBy :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascHasCredential :: Lens.Lens' APNSSandboxChannelResponse (Lude.Maybe Lude.Bool)
ascHasCredential = Lens.lens (hasCredential :: APNSSandboxChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | The type of messaging or notification platform for the channel. For the APNs sandbox channel, this value is APNS_SANDBOX.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascPlatform :: Lens.Lens' APNSSandboxChannelResponse Lude.Text
ascPlatform = Lens.lens (platform :: APNSSandboxChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: APNSSandboxChannelResponse)
{-# DEPRECATED ascPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

instance Lude.FromJSON APNSSandboxChannelResponse where
  parseJSON =
    Lude.withObject
      "APNSSandboxChannelResponse"
      ( \x ->
          APNSSandboxChannelResponse'
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
