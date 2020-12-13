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
    gcmcPlatform,
    gcmcLastModifiedDate,
    gcmcEnabled,
    gcmcCredential,
    gcmcIsArchived,
    gcmcApplicationId,
    gcmcVersion,
    gcmcId,
    gcmcCreationDate,
    gcmcLastModifiedBy,
    gcmcHasCredential,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the GCM channel for an application. The GCM channel enables Amazon Pinpoint to send push notifications through the Firebase Cloud Messaging (FCM), formerly Google Cloud Messaging (GCM), service.
--
-- /See:/ 'mkGCMChannelResponse' smart constructor.
data GCMChannelResponse = GCMChannelResponse'
  { -- | The type of messaging or notification platform for the channel. For the GCM channel, this value is GCM.
    platform :: Lude.Text,
    -- | The date and time when the GCM channel was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Text,
    -- | Specifies whether the GCM channel is enabled for the application.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
    credential :: Lude.Text,
    -- | Specifies whether the GCM channel is archived.
    isArchived :: Lude.Maybe Lude.Bool,
    -- | The unique identifier for the application that the GCM channel applies to.
    applicationId :: Lude.Maybe Lude.Text,
    -- | The current version of the GCM channel.
    version :: Lude.Maybe Lude.Int,
    -- | (Deprecated) An identifier for the GCM channel. This property is retained only for backward compatibility.
    id :: Lude.Maybe Lude.Text,
    -- | The date and time when the GCM channel was enabled.
    creationDate :: Lude.Maybe Lude.Text,
    -- | The user who last modified the GCM channel.
    lastModifiedBy :: Lude.Maybe Lude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GCMChannelResponse' with the minimum fields required to make a request.
--
-- * 'platform' - The type of messaging or notification platform for the channel. For the GCM channel, this value is GCM.
-- * 'lastModifiedDate' - The date and time when the GCM channel was last modified.
-- * 'enabled' - Specifies whether the GCM channel is enabled for the application.
-- * 'credential' - The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
-- * 'isArchived' - Specifies whether the GCM channel is archived.
-- * 'applicationId' - The unique identifier for the application that the GCM channel applies to.
-- * 'version' - The current version of the GCM channel.
-- * 'id' - (Deprecated) An identifier for the GCM channel. This property is retained only for backward compatibility.
-- * 'creationDate' - The date and time when the GCM channel was enabled.
-- * 'lastModifiedBy' - The user who last modified the GCM channel.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
mkGCMChannelResponse ::
  -- | 'platform'
  Lude.Text ->
  -- | 'credential'
  Lude.Text ->
  GCMChannelResponse
mkGCMChannelResponse pPlatform_ pCredential_ =
  GCMChannelResponse'
    { platform = pPlatform_,
      lastModifiedDate = Lude.Nothing,
      enabled = Lude.Nothing,
      credential = pCredential_,
      isArchived = Lude.Nothing,
      applicationId = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      creationDate = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      hasCredential = Lude.Nothing
    }

-- | The type of messaging or notification platform for the channel. For the GCM channel, this value is GCM.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcPlatform :: Lens.Lens' GCMChannelResponse Lude.Text
gcmcPlatform = Lens.lens (platform :: GCMChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The date and time when the GCM channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcLastModifiedDate :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Text)
gcmcLastModifiedDate = Lens.lens (lastModifiedDate :: GCMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the GCM channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcEnabled :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Bool)
gcmcEnabled = Lens.lens (enabled :: GCMChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The Web API Key, also referred to as an /API_KEY/ or /server key/ , that you received from Google to communicate with Google services.
--
-- /Note:/ Consider using 'credential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcCredential :: Lens.Lens' GCMChannelResponse Lude.Text
gcmcCredential = Lens.lens (credential :: GCMChannelResponse -> Lude.Text) (\s a -> s {credential = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcCredential "Use generic-lens or generic-optics with 'credential' instead." #-}

-- | Specifies whether the GCM channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcIsArchived :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Bool)
gcmcIsArchived = Lens.lens (isArchived :: GCMChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the GCM channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcApplicationId :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Text)
gcmcApplicationId = Lens.lens (applicationId :: GCMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the GCM channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcVersion :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Int)
gcmcVersion = Lens.lens (version :: GCMChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the GCM channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcId :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Text)
gcmcId = Lens.lens (id :: GCMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the GCM channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcCreationDate :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Text)
gcmcCreationDate = Lens.lens (creationDate :: GCMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the GCM channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcLastModifiedBy :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Text)
gcmcLastModifiedBy = Lens.lens (lastModifiedBy :: GCMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmcHasCredential :: Lens.Lens' GCMChannelResponse (Lude.Maybe Lude.Bool)
gcmcHasCredential = Lens.lens (hasCredential :: GCMChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: GCMChannelResponse)
{-# DEPRECATED gcmcHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

instance Lude.FromJSON GCMChannelResponse where
  parseJSON =
    Lude.withObject
      "GCMChannelResponse"
      ( \x ->
          GCMChannelResponse'
            Lude.<$> (x Lude..: "Platform")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..: "Credential")
            Lude.<*> (x Lude..:? "IsArchived")
            Lude.<*> (x Lude..:? "ApplicationId")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "HasCredential")
      )
