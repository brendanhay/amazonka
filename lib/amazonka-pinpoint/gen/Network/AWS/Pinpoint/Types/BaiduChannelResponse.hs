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
    bcLastModifiedDate,
    bcEnabled,
    bcIsArchived,
    bcApplicationId,
    bcVersion,
    bcId,
    bcCreationDate,
    bcLastModifiedBy,
    bcHasCredential,
    bcCredential,
    bcPlatform,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about the status and settings of the Baidu (Baidu Cloud Push) channel for an application.
--
-- /See:/ 'mkBaiduChannelResponse' smart constructor.
data BaiduChannelResponse = BaiduChannelResponse'
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

-- | Creates a value of 'BaiduChannelResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The unique identifier for the application that the Baidu channel applies to.
-- * 'creationDate' - The date and time when the Baidu channel was enabled.
-- * 'credential' - The API key that you received from the Baidu Cloud Push service to communicate with the service.
-- * 'enabled' - Specifies whether the Baidu channel is enabled for the application.
-- * 'hasCredential' - (Not used) This property is retained only for backward compatibility.
-- * 'id' - (Deprecated) An identifier for the Baidu channel. This property is retained only for backward compatibility.
-- * 'isArchived' - Specifies whether the Baidu channel is archived.
-- * 'lastModifiedBy' - The user who last modified the Baidu channel.
-- * 'lastModifiedDate' - The date and time when the Baidu channel was last modified.
-- * 'platform' - The type of messaging or notification platform for the channel. For the Baidu channel, this value is BAIDU.
-- * 'version' - The current version of the Baidu channel.
mkBaiduChannelResponse ::
  -- | 'credential'
  Lude.Text ->
  -- | 'platform'
  Lude.Text ->
  BaiduChannelResponse
mkBaiduChannelResponse pCredential_ pPlatform_ =
  BaiduChannelResponse'
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

-- | The API key that you received from the Baidu Cloud Push service to communicate with the service.
--
-- /Note:/ Consider using 'credential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcCredential :: Lens.Lens' BaiduChannelResponse Lude.Text
bcCredential = Lens.lens (credential :: BaiduChannelResponse -> Lude.Text) (\s a -> s {credential = a} :: BaiduChannelResponse)
{-# DEPRECATED bcCredential "Use generic-lens or generic-optics with 'credential' instead." #-}

-- | The type of messaging or notification platform for the channel. For the Baidu channel, this value is BAIDU.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcPlatform :: Lens.Lens' BaiduChannelResponse Lude.Text
bcPlatform = Lens.lens (platform :: BaiduChannelResponse -> Lude.Text) (\s a -> s {platform = a} :: BaiduChannelResponse)
{-# DEPRECATED bcPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

instance Lude.FromJSON BaiduChannelResponse where
  parseJSON =
    Lude.withObject
      "BaiduChannelResponse"
      ( \x ->
          BaiduChannelResponse'
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
