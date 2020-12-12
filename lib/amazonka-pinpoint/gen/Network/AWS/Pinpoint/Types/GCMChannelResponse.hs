{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GCMChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GCMChannelResponse
  ( GCMChannelResponse (..),

    -- * Smart constructor
    mkGCMChannelResponse,

    -- * Lenses
    gcLastModifiedDate,
    gcEnabled,
    gcIsArchived,
    gcApplicationId,
    gcVersion,
    gcId,
    gcCreationDate,
    gcLastModifiedBy,
    gcHasCredential,
    gcCredential,
    gcPlatform,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the GCM channel for an application. The GCM channel enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
--
-- /See:/ 'mkGCMChannelResponse' smart constructor.
data GCMChannelResponse = GCMChannelResponse'
  { lastModifiedDate ::
      Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    isArchived :: Lude.Maybe Lude.Bool,
    applicationId :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Int,
    id :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe Lude.Text,
    hasCredential :: Lude.Maybe Lude.Bool,
    credential :: Lude.Text,
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

-- | Creates a value of 'GCMChannelResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the GCM channel applies to.
-- * 'creationDate' - The date and time when the GCM channel was enabled.
-- * 'credential' - The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
-- * 'enabled' - Specifies whether the GCM channel is enabled for the application.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
-- * 'id' - (Deprecated) An identifier for the GCM channel. This property is retained only for backward compatibility.
-- * 'isArchived' - Specifies whether the GCM channel is archived.
-- * 'lastModifiedBy' - The user who last modified the GCM channel.
-- * 'lastModifiedDate' - The date and time when the GCM channel was last modified.
-- * 'platform' - The type of messaging or notification platform for the channel. For the GCM channel, this value is GCM.
-- * 'version' - The current version of the GCM channel.
mkGCMChannelResponse ::
  -- | 'credential'
  Lude.Text ->
  -- | 'platform'
  Lude.Text ->
  GCMChannelResponse
mkGCMChannelResponse pCredential_ pPlatform_ =
  GCMChannelResponse'
    { lastModifiedDate = Lude.Nothing,
      enabled = Lude.Nothing,
      isArchived = Lude.Nothing,
      applicationId = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      creationDate = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      hasCredential = Lude.Nothing,
      credential = pCredential_,
      platform = pPlatform_
    }

-- | The date and time when the GCM channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcLastModifiedDate :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Text)
gcLastModifiedDate = Lens.lens (lastModifiedDate :: GCMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: GCMChannelResponse)
{-# DEPRECATED gcLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the GCM channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcEnabled :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Bool)
gcEnabled = Lens.lens (enabled :: GCMChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: GCMChannelResponse)
{-# DEPRECATED gcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether the GCM channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcIsArchived :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Bool)
gcIsArchived = Lens.lens (isArchived :: GCMChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: GCMChannelResponse)
{-# DEPRECATED gcIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the GCM channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcApplicationId :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Text)
gcApplicationId = Lens.lens (applicationId :: GCMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: GCMChannelResponse)
{-# DEPRECATED gcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the GCM channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcVersion :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Int)
gcVersion = Lens.lens (version :: GCMChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: GCMChannelResponse)
{-# DEPRECATED gcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the GCM channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcId :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Text)
gcId = Lens.lens (id :: GCMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GCMChannelResponse)
{-# DEPRECATED gcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the GCM channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCreationDate :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Text)
gcCreationDate = Lens.lens (creationDate :: GCMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: GCMChannelResponse)
{-# DEPRECATED gcCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the GCM channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcLastModifiedBy :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Text)
gcLastModifiedBy = Lens.lens (lastModifiedBy :: GCMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: GCMChannelResponse)
{-# DEPRECATED gcLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcHasCredential :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Bool)
gcHasCredential = Lens.lens (hasCredential :: GCMChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: GCMChannelResponse)
{-# DEPRECATED gcHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
--
-- /Note:/ Consider using 'credential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCredential :: Lens.Lens' GCMChannelResponse Lude.Text
gcCredential = Lens.lens (credential :: GCMChannelResponse -> Lude.Text) (\s a -> s {credential = a} :: GCMChannelResponse)
{-# DEPRECATED gcCredential "Use generic-lens or generic-optics with 'credential' instead." #-}

-- | The type of messaging or notification platform for the channel. For the GCM channel, this value is GCM.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcPlatform :: Lens.Lens' GCMChannelResponse Lude.Text
gcPlatform = Lens.lens (platform :: GCMChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: GCMChannelResponse)
{-# DEPRECATED gcPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

instance Lude.FromJSON GCMChannelResponse where
  parseJSON =
    Lude.withObject
      "GCMChannelResponse"
      ( \x ->
          GCMChannelResponse'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "IsArchived")
            Lude.<*> (x Lude..:? "ApplicationId")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "HasCredential")
            Lude.<*> (x Lude..: "Credential")
            Lude.<*> (x Lude..: "Platform")
      )
