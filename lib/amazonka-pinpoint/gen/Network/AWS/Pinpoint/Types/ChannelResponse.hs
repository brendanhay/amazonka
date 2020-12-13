{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.ChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.ChannelResponse
  ( ChannelResponse (..),

    -- * Smart constructor
    mkChannelResponse,

    -- * Lenses
    cfLastModifiedDate,
    cfEnabled,
    cfIsArchived,
    cfApplicationId,
    cfVersion,
    cfId,
    cfCreationDate,
    cfLastModifiedBy,
    cfHasCredential,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the general settings and status of a channel for an application.
--
-- /See:/ 'mkChannelResponse' smart constructor.
data ChannelResponse = ChannelResponse'
  { -- | The date and time, in ISO 8601 format, when the channel was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Text,
    -- | Specifies whether the channel is enabled for the application.
    enabled :: Lude.Maybe Lude.Bool,
    -- | Specifies whether the channel is archived.
    isArchived :: Lude.Maybe Lude.Bool,
    -- | The unique identifier for the application.
    applicationId :: Lude.Maybe Lude.Text,
    -- | The current version of the channel.
    version :: Lude.Maybe Lude.Int,
    -- | (Deprecated) An identifier for the channel. This property is retained only for backward compatibility.
    id :: Lude.Maybe Lude.Text,
    -- | The date and time, in ISO 8601 format, when the channel was enabled.
    creationDate :: Lude.Maybe Lude.Text,
    -- | The user who last modified the channel.
    lastModifiedBy :: Lude.Maybe Lude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelResponse' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - The date and time, in ISO 8601 format, when the channel was last modified.
-- * 'enabled' - Specifies whether the channel is enabled for the application.
-- * 'isArchived' - Specifies whether the channel is archived.
-- * 'applicationId' - The unique identifier for the application.
-- * 'version' - The current version of the channel.
-- * 'id' - (Deprecated) An identifier for the channel. This property is retained only for backward compatibility.
-- * 'creationDate' - The date and time, in ISO 8601 format, when the channel was enabled.
-- * 'lastModifiedBy' - The user who last modified the channel.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
mkChannelResponse ::
  ChannelResponse
mkChannelResponse =
  ChannelResponse'
    { lastModifiedDate = Lude.Nothing,
      enabled = Lude.Nothing,
      isArchived = Lude.Nothing,
      applicationId = Lude.Nothing,
      version = Lude.Nothing,
      id = Lude.Nothing,
      creationDate = Lude.Nothing,
      lastModifiedBy = Lude.Nothing,
      hasCredential = Lude.Nothing
    }

-- | The date and time, in ISO 8601 format, when the channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLastModifiedDate :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Text)
cfLastModifiedDate = Lens.lens (lastModifiedDate :: ChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: ChannelResponse)
{-# DEPRECATED cfLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfEnabled :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Bool)
cfEnabled = Lens.lens (enabled :: ChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ChannelResponse)
{-# DEPRECATED cfEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether the channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfIsArchived :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Bool)
cfIsArchived = Lens.lens (isArchived :: ChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: ChannelResponse)
{-# DEPRECATED cfIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfApplicationId :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Text)
cfApplicationId = Lens.lens (applicationId :: ChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: ChannelResponse)
{-# DEPRECATED cfApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfVersion :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Int)
cfVersion = Lens.lens (version :: ChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: ChannelResponse)
{-# DEPRECATED cfVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfId :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Text)
cfId = Lens.lens (id :: ChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ChannelResponse)
{-# DEPRECATED cfId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time, in ISO 8601 format, when the channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfCreationDate :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Text)
cfCreationDate = Lens.lens (creationDate :: ChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: ChannelResponse)
{-# DEPRECATED cfCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfLastModifiedBy :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Text)
cfLastModifiedBy = Lens.lens (lastModifiedBy :: ChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: ChannelResponse)
{-# DEPRECATED cfLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfHasCredential :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Bool)
cfHasCredential = Lens.lens (hasCredential :: ChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: ChannelResponse)
{-# DEPRECATED cfHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

instance Lude.FromJSON ChannelResponse where
  parseJSON =
    Lude.withObject
      "ChannelResponse"
      ( \x ->
          ChannelResponse'
            Lude.<$> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "IsArchived")
            Lude.<*> (x Lude..:? "ApplicationId")
            Lude.<*> (x Lude..:? "Version")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LastModifiedBy")
            Lude.<*> (x Lude..:? "HasCredential")
      )
