-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ADMChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ADMChannelResponse
  ( ADMChannelResponse (..),

    -- * Smart constructor
    mkADMChannelResponse,

    -- * Lenses
    admcLastModifiedDate,
    admcEnabled,
    admcIsArchived,
    admcApplicationId,
    admcVersion,
    admcId,
    admcCreationDate,
    admcLastModifiedBy,
    admcHasCredential,
    admcPlatform,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the ADM (Amazon Device Messaging) channel for an application.
--
-- /See:/ 'mkADMChannelResponse' smart constructor.
data ADMChannelResponse = ADMChannelResponse'
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

-- | Creates a value of 'ADMChannelResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the ADM channel applies to.
-- * 'creationDate' - The date and time when the ADM channel was enabled.
-- * 'enabled' - Specifies whether the ADM channel is enabled for the application.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
-- * 'id' - (Deprecated) An identifier for the ADM channel. This property is retained only for backward compatibility.
-- * 'isArchived' - Specifies whether the ADM channel is archived.
-- * 'lastModifiedBy' - The user who last modified the ADM channel.
-- * 'lastModifiedDate' - The date and time when the ADM channel was last modified.
-- * 'platform' - The type of messaging or notification platform for the channel. For the ADM channel, this value is ADM.
-- * 'version' - The current version of the ADM channel.
mkADMChannelResponse ::
  -- | 'platform'
  Lude.Text ->
  ADMChannelResponse
mkADMChannelResponse pPlatform_ =
  ADMChannelResponse'
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

-- | The date and time when the ADM channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcLastModifiedDate :: Lens.Lens' ADMChannelResponse (Lude.Maybe Lude.Text)
admcLastModifiedDate = Lens.lens (lastModifiedDate :: ADMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: ADMChannelResponse)
{-# DEPRECATED admcLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the ADM channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcEnabled :: Lens.Lens' ADMChannelResponse (Lude.Maybe Lude.Bool)
admcEnabled = Lens.lens (enabled :: ADMChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ADMChannelResponse)
{-# DEPRECATED admcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether the ADM channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcIsArchived :: Lens.Lens' ADMChannelResponse (Lude.Maybe Lude.Bool)
admcIsArchived = Lens.lens (isArchived :: ADMChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: ADMChannelResponse)
{-# DEPRECATED admcIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the ADM channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcApplicationId :: Lens.Lens' ADMChannelResponse (Lude.Maybe Lude.Text)
admcApplicationId = Lens.lens (applicationId :: ADMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: ADMChannelResponse)
{-# DEPRECATED admcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the ADM channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcVersion :: Lens.Lens' ADMChannelResponse (Lude.Maybe Lude.Int)
admcVersion = Lens.lens (version :: ADMChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: ADMChannelResponse)
{-# DEPRECATED admcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the ADM channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcId :: Lens.Lens' ADMChannelResponse (Lude.Maybe Lude.Text)
admcId = Lens.lens (id :: ADMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ADMChannelResponse)
{-# DEPRECATED admcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the ADM channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcCreationDate :: Lens.Lens' ADMChannelResponse (Lude.Maybe Lude.Text)
admcCreationDate = Lens.lens (creationDate :: ADMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: ADMChannelResponse)
{-# DEPRECATED admcCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the ADM channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcLastModifiedBy :: Lens.Lens' ADMChannelResponse (Lude.Maybe Lude.Text)
admcLastModifiedBy = Lens.lens (lastModifiedBy :: ADMChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: ADMChannelResponse)
{-# DEPRECATED admcLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcHasCredential :: Lens.Lens' ADMChannelResponse (Lude.Maybe Lude.Bool)
admcHasCredential = Lens.lens (hasCredential :: ADMChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: ADMChannelResponse)
{-# DEPRECATED admcHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

-- | The type of messaging or notification platform for the channel. For the ADM channel, this value is ADM.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
admcPlatform :: Lens.Lens' ADMChannelResponse Lude.Text
admcPlatform = Lens.lens (platform :: ADMChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: ADMChannelResponse)
{-# DEPRECATED admcPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

instance Lude.FromJSON ADMChannelResponse where
  parseJSON =
    Lude.withObject
      "ADMChannelResponse"
      ( \x ->
          ADMChannelResponse'
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
