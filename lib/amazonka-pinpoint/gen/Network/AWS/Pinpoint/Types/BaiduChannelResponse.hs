{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.BaiduChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.BaiduChannelResponse
  ( BaiduChannelResponse (..),

    -- * Smart constructor
    mkBaiduChannelResponse,

    -- * Lenses
    bcPlatform,
    bcLastModifiedDate,
    bcEnabled,
    bcCredential,
    bcIsArchived,
    bcApplicationId,
    bcVersion,
    bcId,
    bcCreationDate,
    bcLastModifiedBy,
    bcHasCredential,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the Baidu (Baidu Cloud Push) channel for an application.
--
-- /See:/ 'mkBaiduChannelResponse' smart constructor.
data BaiduChannelResponse = BaiduChannelResponse'
  { -- | The type of messaging or notification platform for the channel. For the Baidu channel, this value is BAIDU.
    platform :: Lude.Text,
    -- | The date and time when the Baidu channel was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Text,
    -- | Specifies whether the Baidu channel is enabled for the application.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The API key that you received from the Baidu Cloud Push service to communicate with the service.
    credential :: Lude.Text,
    -- | Specifies whether the Baidu channel is archived.
    isArchived :: Lude.Maybe Lude.Bool,
    -- | The unique identifier for the application that the Baidu channel applies to.
    applicationId :: Lude.Maybe Lude.Text,
    -- | The current version of the Baidu channel.
    version :: Lude.Maybe Lude.Int,
    -- | (Deprecated) An identifier for the Baidu channel. This property is retained only for backward compatibility.
    id :: Lude.Maybe Lude.Text,
    -- | The date and time when the Baidu channel was enabled.
    creationDate :: Lude.Maybe Lude.Text,
    -- | The user who last modified the Baidu channel.
    lastModifiedBy :: Lude.Maybe Lude.Text,
    -- | (Not used) This property is retained only for backward compatibility.
    hasCredential :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BaiduChannelResponse' with the minimum fields required to make a request.
--
-- * 'platform' - The type of messaging or notification platform for the channel. For the Baidu channel, this value is BAIDU.
-- * 'lastModifiedDate' - The date and time when the Baidu channel was last modified.
-- * 'enabled' - Specifies whether the Baidu channel is enabled for the application.
-- * 'credential' - The API key that you received from the Baidu Cloud Push service to communicate with the service.
-- * 'isArchived' - Specifies whether the Baidu channel is archived.
-- * 'applicationId' - The unique identifier for the application that the Baidu channel applies to.
-- * 'version' - The current version of the Baidu channel.
-- * 'id' - (Deprecated) An identifier for the Baidu channel. This property is retained only for backward compatibility.
-- * 'creationDate' - The date and time when the Baidu channel was enabled.
-- * 'lastModifiedBy' - The user who last modified the Baidu channel.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
mkBaiduChannelResponse ::
  -- | 'platform'
  Lude.Text ->
  -- | 'credential'
  Lude.Text ->
  BaiduChannelResponse
mkBaiduChannelResponse pPlatform_ pCredential_ =
  BaiduChannelResponse'
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

-- | The type of messaging or notification platform for the channel. For the Baidu channel, this value is BAIDU.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcPlatform :: Lens.Lens' BaiduChannelResponse Lude.Text
bcPlatform = Lens.lens (platform :: BaiduChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: BaiduChannelResponse)
{-# DEPRECATED bcPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The date and time when the Baidu channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcLastModifiedDate :: Lens.Lens' BaiduChannelResponse (Lude.Maybe Lude.Text)
bcLastModifiedDate = Lens.lens (lastModifiedDate :: BaiduChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedDate = a} :: BaiduChannelResponse)
{-# DEPRECATED bcLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Specifies whether the Baidu channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcEnabled :: Lens.Lens' BaiduChannelResponse (Lude.Maybe Lude.Bool)
bcEnabled = Lens.lens (enabled :: BaiduChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: BaiduChannelResponse)
{-# DEPRECATED bcEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The API key that you received from the Baidu Cloud Push service to communicate with the service.
--
-- /Note:/ Consider using 'credential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcCredential :: Lens.Lens' BaiduChannelResponse Lude.Text
bcCredential = Lens.lens (credential :: BaiduChannelResponse -> Lude.Text) (\s a -> s {credential = a} :: BaiduChannelResponse)
{-# DEPRECATED bcCredential "Use generic-lens or generic-optics with 'credential' instead." #-}

-- | Specifies whether the Baidu channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcIsArchived :: Lens.Lens' BaiduChannelResponse (Lude.Maybe Lude.Bool)
bcIsArchived = Lens.lens (isArchived :: BaiduChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isArchived = a} :: BaiduChannelResponse)
{-# DEPRECATED bcIsArchived "Use generic-lens or generic-optics with 'isArchived' instead." #-}

-- | The unique identifier for the application that the Baidu channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcApplicationId :: Lens.Lens' BaiduChannelResponse (Lude.Maybe Lude.Text)
bcApplicationId = Lens.lens (applicationId :: BaiduChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: BaiduChannelResponse)
{-# DEPRECATED bcApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The current version of the Baidu channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcVersion :: Lens.Lens' BaiduChannelResponse (Lude.Maybe Lude.Int)
bcVersion = Lens.lens (version :: BaiduChannelResponse -> Lude.Maybe Lude.Int) (\s a -> s {version = a} :: BaiduChannelResponse)
{-# DEPRECATED bcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | (Deprecated) An identifier for the Baidu channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcId :: Lens.Lens' BaiduChannelResponse (Lude.Maybe Lude.Text)
bcId = Lens.lens (id :: BaiduChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: BaiduChannelResponse)
{-# DEPRECATED bcId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time when the Baidu channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcCreationDate :: Lens.Lens' BaiduChannelResponse (Lude.Maybe Lude.Text)
bcCreationDate = Lens.lens (creationDate :: BaiduChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationDate = a} :: BaiduChannelResponse)
{-# DEPRECATED bcCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The user who last modified the Baidu channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcLastModifiedBy :: Lens.Lens' BaiduChannelResponse (Lude.Maybe Lude.Text)
bcLastModifiedBy = Lens.lens (lastModifiedBy :: BaiduChannelResponse -> Lude.Maybe Lude.Text) (\s a -> s {lastModifiedBy = a} :: BaiduChannelResponse)
{-# DEPRECATED bcLastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead." #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcHasCredential :: Lens.Lens' BaiduChannelResponse (Lude.Maybe Lude.Bool)
bcHasCredential = Lens.lens (hasCredential :: BaiduChannelResponse -> Lude.Maybe Lude.Bool) (\s a -> s {hasCredential = a} :: BaiduChannelResponse)
{-# DEPRECATED bcHasCredential "Use generic-lens or generic-optics with 'hasCredential' instead." #-}

instance Lude.FromJSON BaiduChannelResponse where
  parseJSON =
    Lude.withObject
      "BaiduChannelResponse"
      ( \x ->
          BaiduChannelResponse'
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
