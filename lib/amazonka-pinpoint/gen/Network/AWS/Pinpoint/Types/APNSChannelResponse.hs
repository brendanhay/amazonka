{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.APNSChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.APNSChannelResponse
  ( APNSChannelResponse (..),

    -- * Smart constructor
    mkAPNSChannelResponse,

    -- * Lenses
    acLastModifiedDate,
    acEnabled,
    acHasTokenKey,
    acDefaultAuthenticationMethod,
    acIsArchived,
    acApplicationId,
    acVersion,
    acId,
    acCreationDate,
    acLastModifiedBy,
    acHasCredential,
    acPlatform,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the APNs (Apple Push Notification service) channel for an application.
--
-- /See:/ 'mkAPNSChannelResponse' smart constructor.
data APNSChannelResponse = APNSChannelResponse'
  { lastModifiedDate ::
      Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    hasTokenKey :: Lude.Maybe Lude.Bool,
    defaultAuthenticationMethod :: Lude.Maybe Lude.Text,
    isArchived :: Lude.Maybe Lude.Bool,
    applicationId :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Int,
    id :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'APNSChannelResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the APNs channel applies to.
-- * 'creationDate' - The date and time when the APNs channel was enabled.
-- * 'defaultAuthenticationMethod' - The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
-- * 'enabled' - Specifies whether the APNs channel is enabled for the application.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
-- * 'hasTokenKey' - Specifies whether the APNs channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
-- * 'id' - (Deprecated) An identifier for the APNs channel. This property is retained only for backward compatibility.
-- * 'isArchived' - Specifies whether the APNs channel is archived.
-- * 'lastModifiedBy' - The user who last modified the APNs channel.
-- * 'lastModifiedDate' - The date and time when the APNs channel was last modified.
-- * 'platform' - The type of messaging or notification platform for the channel. For the APNs channel, this value is APNS.
-- * 'version' - The current version of the APNs channel.
mkAPNSChannelResponse ::
  -- | 'platform'
  Lude.Text ->
  APNSChannelResponse
mkAPNSChannelResponse pPlatform_ =
  APNSChannelResponse'
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

-- | The date and time when the APNs channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acLastModifiedDate :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Text)
acLastModifiedDate = Lens.lens (lastModifiedDate :: APNSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: APNSChannelResponse)
{-# DEPRECATED acLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the APNs channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acEnabled :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Bool)
acEnabled = Lens.lens (enabled :: APNSChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: APNSChannelResponse)
{-# DEPRECATED acEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether the APNs channel is configured to communicate with APNs by using APNs tokens. To provide an authentication key for APNs tokens, set the TokenKey property of the channel.
--
-- /Note:/ Consider using 'hasTokenKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acHasTokenKey :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Bool)
acHasTokenKey = Lens.lens (hasTokenKey :: APNSChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasTokenKey = a} :: APNSChannelResponse)
{-# DEPRECATED acHasTokenKey "Use generic-lens or generic-optics with 'hasTokenKey' instead." #-}

-- | The default authentication method that Amazon Pinpoint uses to authenticate with APNs for this channel, key or certificate.
--
-- /Note:/ Consider using 'defaultAuthenticationMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acDefaultAuthenticationMethod :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Text)
acDefaultAuthenticationMethod = Lens.lens (defaultAuthenticationMethod :: APNSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {defaultAuthenticationMethod = a} :: APNSChannelResponse)
{-# DEPRECATED acDefaultAuthenticationMethod "Use generic-lens or generic-optics with 'defaultAuthenticationMethod' instead." #-}

-- | Specifies whether the APNs channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acIsArchived :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Bool)
acIsArchived = Lens.lens (isArchived :: APNSChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: APNSChannelResponse)
{-# DEPRECATED acIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the APNs channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acApplicationId :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Text)
acApplicationId = Lens.lens (applicationId :: APNSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: APNSChannelResponse)
{-# DEPRECATED acApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the APNs channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acVersion :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Int)
acVersion = Lens.lens (version :: APNSChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: APNSChannelResponse)
{-# DEPRECATED acVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the APNs channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acId :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Text)
acId = Lens.lens (id :: APNSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: APNSChannelResponse)
{-# DEPRECATED acId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the APNs channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acCreationDate :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Text)
acCreationDate = Lens.lens (creationDate :: APNSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: APNSChannelResponse)
{-# DEPRECATED acCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the APNs channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acLastModifiedBy :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Text)
acLastModifiedBy = Lens.lens (lastModifiedBy :: APNSChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: APNSChannelResponse)
{-# DEPRECATED acLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acHasCredential :: Lens.Lens' APNSChannelResponse (Lude.Maybe Lude.Bool)
acHasCredential = Lens.lens (hasCredential :: APNSChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: APNSChannelResponse)
{-# DEPRECATED acHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | The type of messaging or notification platform for the channel. For the APNs channel, this value is APNS.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acPlatform :: Lens.Lens' APNSChannelResponse Lude.Text
acPlatform = Lens.lens (platform :: APNSChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: APNSChannelResponse)
{-# DEPRECATED acPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

instance Lude.FromJSON APNSChannelResponse where
  parseJSON =
    Lude.withObject
      "APNSChannelResponse"
      ( \x ->
          APNSChannelResponse'
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
