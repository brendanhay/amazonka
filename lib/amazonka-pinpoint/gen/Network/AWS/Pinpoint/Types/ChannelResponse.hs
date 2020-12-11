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
    chaLastModifiedDate,
    chaEnabled,
    chaIsArchived,
    chaApplicationId,
    chaVersion,
    chaId,
    chaCreationDate,
    chaLastModifiedBy,
    chaHasCredential,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the general settings and status of a channel for an application.
--
-- /See:/ 'mkChannelResponse' smart constructor.
data ChannelResponse = ChannelResponse'
  { lastModifiedDate ::
      Lude.Maybe Lude.Text,
    enabled :: Lude.Maybe Lude.Bool,
    isArchived :: Lude.Maybe Lude.Bool,
    applicationId :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Int,
    id :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Text,
    lastModifiedBy :: Lude.Maybe Lude.Text,
    hasCredential :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChannelResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application.
-- * 'creationDate' - The date and time, in ISO 8601 format, when the channel was enabled.
-- * 'enabled' - Specifies whether the channel is enabled for the application.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
-- * 'id' - (Deprecated) An identifier for the channel. This property is retained only for backward compatibility.
-- * 'isArchived' - Specifies whether the channel is archived.
-- * 'lastModifiedBy' - The user who last modified the channel.
-- * 'lastModifiedDate' - The date and time, in ISO 8601 format, when the channel was last modified.
-- * 'version' - The current version of the channel.
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
chaLastModifiedDate :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Text)
chaLastModifiedDate = Lens.lens (lastModifiedDate :: ChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: ChannelResponse)
{-# DEPRECATED chaLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaEnabled :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Bool)
chaEnabled = Lens.lens (enabled :: ChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ChannelResponse)
{-# DEPRECATED chaEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies whether the channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaIsArchived :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Bool)
chaIsArchived = Lens.lens (isArchived :: ChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: ChannelResponse)
{-# DEPRECATED chaIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaApplicationId :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Text)
chaApplicationId = Lens.lens (applicationId :: ChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: ChannelResponse)
{-# DEPRECATED chaApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaVersion :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Int)
chaVersion = Lens.lens (version :: ChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: ChannelResponse)
{-# DEPRECATED chaVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaId :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Text)
chaId = Lens.lens (id :: ChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: ChannelResponse)
{-# DEPRECATED chaId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time, in ISO 8601 format, when the channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaCreationDate :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Text)
chaCreationDate = Lens.lens (creationDate :: ChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: ChannelResponse)
{-# DEPRECATED chaCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaLastModifiedBy :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Text)
chaLastModifiedBy = Lens.lens (lastModifiedBy :: ChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: ChannelResponse)
{-# DEPRECATED chaLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
chaHasCredential :: Lens.Lens' ChannelResponse (Lude.Maybe Lude.Bool)
chaHasCredential = Lens.lens (hasCredential :: ChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: ChannelResponse)
{-# DEPRECATED chaHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

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
