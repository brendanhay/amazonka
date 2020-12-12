{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceChannelResponse
  ( VoiceChannelResponse (..),

    -- * Smart constructor
    mkVoiceChannelResponse,

    -- * Lenses
    vcLastModifiedDate,
    vcEnabled,
    vcIsArchived,
    vcApplicationId,
    vcVersion,
    vcId,
    vcCreationDate,
    vcLastModifiedBy,
    vcHasCredential,
    vcPlatform,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the voice channel for an application.
--
-- /See:/ 'mkVoiceChannelResponse' smart constructor.
data VoiceChannelResponse = VoiceChannelResponse'
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

-- | Creates a value of 'VoiceChannelResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the voice channel applies to.
-- * 'creationDate' - The date and time, in ISO 8601 format, when the voice channel was enabled.
-- * 'enabled' - Specifies whether the voice channel is enabled for the application.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
-- * 'id' - (Deprecated) An identifier for the voice channel. This property is retained only for backward compatibility.
-- * 'isArchived' - Specifies whether the voice channel is archived.
-- * 'lastModifiedBy' - The user who last modified the voice channel.
-- * 'lastModifiedDate' - The date and time, in ISO 8601 format, when the voice channel was last modified.
-- * 'platform' - The type of messaging or notification platform for the channel. For the voice channel, this value is VOICE.
-- * 'version' - The current version of the voice channel.
mkVoiceChannelResponse ::
  -- | 'platform'
  Lude.Text ->
  VoiceChannelResponse
mkVoiceChannelResponse pPlatform_ =
  VoiceChannelResponse'
    { lastModifiedDate = Lude.Nothing,
      enabled = Lude.Nothing,
      isArchived = Lude.Nothing,
      applicationId = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      creationDate = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      hasCredential = Lude.Nothing,
      platform = pPlatform_
    }

-- | The date and time, in ISO 8601 format, when the voice channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcLastModifiedDate :: Lens.Lens' VoiceChannelResponse (Lude.Maybe Lude.Text)
vcLastModifiedDate = Lens.lens (lastModifiedDate :: VoiceChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: VoiceChannelResponse)
{-# DEPRECATED vcLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the voice channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcEnabled :: Lens.Lens' VoiceChannelResponse (Lude.Maybe Lude.Bool)
vcEnabled = Lens.lens (enabled :: VoiceChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: VoiceChannelResponse)
{-# DEPRECATED vcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether the voice channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcIsArchived :: Lens.Lens' VoiceChannelResponse (Lude.Maybe Lude.Bool)
vcIsArchived = Lens.lens (isArchived :: VoiceChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: VoiceChannelResponse)
{-# DEPRECATED vcIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the voice channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcApplicationId :: Lens.Lens' VoiceChannelResponse (Lude.Maybe Lude.Text)
vcApplicationId = Lens.lens (applicationId :: VoiceChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: VoiceChannelResponse)
{-# DEPRECATED vcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the voice channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcVersion :: Lens.Lens' VoiceChannelResponse (Lude.Maybe Lude.Int)
vcVersion = Lens.lens (version :: VoiceChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: VoiceChannelResponse)
{-# DEPRECATED vcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the voice channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcId :: Lens.Lens' VoiceChannelResponse (Lude.Maybe Lude.Text)
vcId = Lens.lens (id :: VoiceChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: VoiceChannelResponse)
{-# DEPRECATED vcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time, in ISO 8601 format, when the voice channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcCreationDate :: Lens.Lens' VoiceChannelResponse (Lude.Maybe Lude.Text)
vcCreationDate = Lens.lens (creationDate :: VoiceChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: VoiceChannelResponse)
{-# DEPRECATED vcCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the voice channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcLastModifiedBy :: Lens.Lens' VoiceChannelResponse (Lude.Maybe Lude.Text)
vcLastModifiedBy = Lens.lens (lastModifiedBy :: VoiceChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: VoiceChannelResponse)
{-# DEPRECATED vcLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcHasCredential :: Lens.Lens' VoiceChannelResponse (Lude.Maybe Lude.Bool)
vcHasCredential = Lens.lens (hasCredential :: VoiceChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: VoiceChannelResponse)
{-# DEPRECATED vcHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | The type of messaging or notification platform for the channel. For the voice channel, this value is VOICE.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vcPlatform :: Lens.Lens' VoiceChannelResponse Lude.Text
vcPlatform = Lens.lens (platform :: VoiceChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: VoiceChannelResponse)
{-# DEPRECATED vcPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

instance Lude.FromJSON VoiceChannelResponse where
  parseJSON =
    Lude.withObject
      "VoiceChannelResponse"
      ( \x ->
          VoiceChannelResponse'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "IsArchived")
            Lude.<*> (x Lude..:? "ApplicationId")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "HasCredential")
            Lude.<*> (x Lude..: "Platform")
      )
