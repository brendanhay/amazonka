{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.BaiduChannelResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.BaiduChannelResponse
  ( BaiduChannelResponse (..)
  -- * Smart constructor
  , mkBaiduChannelResponse
  -- * Lenses
  , bcrCredential
  , bcrPlatform
  , bcrApplicationId
  , bcrCreationDate
  , bcrEnabled
  , bcrHasCredential
  , bcrId
  , bcrIsArchived
  , bcrLastModifiedBy
  , bcrLastModifiedDate
  , bcrVersion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about the status and settings of the Baidu (Baidu Cloud Push) channel for an application.
--
-- /See:/ 'mkBaiduChannelResponse' smart constructor.
data BaiduChannelResponse = BaiduChannelResponse'
  { credential :: Core.Text
    -- ^ The API key that you received from the Baidu Cloud Push service to communicate with the service.
  , platform :: Core.Text
    -- ^ The type of messaging or notification platform for the channel. For the Baidu channel, this value is BAIDU.
  , applicationId :: Core.Maybe Core.Text
    -- ^ The unique identifier for the application that the Baidu channel applies to.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The date and time when the Baidu channel was enabled.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the Baidu channel is enabled for the application.
  , hasCredential :: Core.Maybe Core.Bool
    -- ^ (Not used) This property is retained only for backward compatibility.
  , id :: Core.Maybe Core.Text
    -- ^ (Deprecated) An identifier for the Baidu channel. This property is retained only for backward compatibility.
  , isArchived :: Core.Maybe Core.Bool
    -- ^ Specifies whether the Baidu channel is archived.
  , lastModifiedBy :: Core.Maybe Core.Text
    -- ^ The user who last modified the Baidu channel.
  , lastModifiedDate :: Core.Maybe Core.Text
    -- ^ The date and time when the Baidu channel was last modified.
  , version :: Core.Maybe Core.Int
    -- ^ The current version of the Baidu channel.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BaiduChannelResponse' value with any optional fields omitted.
mkBaiduChannelResponse
    :: Core.Text -- ^ 'credential'
    -> Core.Text -- ^ 'platform'
    -> BaiduChannelResponse
mkBaiduChannelResponse credential platform
  = BaiduChannelResponse'{credential, platform,
                          applicationId = Core.Nothing, creationDate = Core.Nothing,
                          enabled = Core.Nothing, hasCredential = Core.Nothing,
                          id = Core.Nothing, isArchived = Core.Nothing,
                          lastModifiedBy = Core.Nothing, lastModifiedDate = Core.Nothing,
                          version = Core.Nothing}

-- | The API key that you received from the Baidu Cloud Push service to communicate with the service.
--
-- /Note:/ Consider using 'credential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrCredential :: Lens.Lens' BaiduChannelResponse Core.Text
bcrCredential = Lens.field @"credential"
{-# INLINEABLE bcrCredential #-}
{-# DEPRECATED credential "Use generic-lens or generic-optics with 'credential' instead"  #-}

-- | The type of messaging or notification platform for the channel. For the Baidu channel, this value is BAIDU.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrPlatform :: Lens.Lens' BaiduChannelResponse Core.Text
bcrPlatform = Lens.field @"platform"
{-# INLINEABLE bcrPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The unique identifier for the application that the Baidu channel applies to.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrApplicationId :: Lens.Lens' BaiduChannelResponse (Core.Maybe Core.Text)
bcrApplicationId = Lens.field @"applicationId"
{-# INLINEABLE bcrApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The date and time when the Baidu channel was enabled.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrCreationDate :: Lens.Lens' BaiduChannelResponse (Core.Maybe Core.Text)
bcrCreationDate = Lens.field @"creationDate"
{-# INLINEABLE bcrCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | Specifies whether the Baidu channel is enabled for the application.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrEnabled :: Lens.Lens' BaiduChannelResponse (Core.Maybe Core.Bool)
bcrEnabled = Lens.field @"enabled"
{-# INLINEABLE bcrEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | (Not used) This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'hasCredential' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrHasCredential :: Lens.Lens' BaiduChannelResponse (Core.Maybe Core.Bool)
bcrHasCredential = Lens.field @"hasCredential"
{-# INLINEABLE bcrHasCredential #-}
{-# DEPRECATED hasCredential "Use generic-lens or generic-optics with 'hasCredential' instead"  #-}

-- | (Deprecated) An identifier for the Baidu channel. This property is retained only for backward compatibility.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrId :: Lens.Lens' BaiduChannelResponse (Core.Maybe Core.Text)
bcrId = Lens.field @"id"
{-# INLINEABLE bcrId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | Specifies whether the Baidu channel is archived.
--
-- /Note:/ Consider using 'isArchived' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrIsArchived :: Lens.Lens' BaiduChannelResponse (Core.Maybe Core.Bool)
bcrIsArchived = Lens.field @"isArchived"
{-# INLINEABLE bcrIsArchived #-}
{-# DEPRECATED isArchived "Use generic-lens or generic-optics with 'isArchived' instead"  #-}

-- | The user who last modified the Baidu channel.
--
-- /Note:/ Consider using 'lastModifiedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrLastModifiedBy :: Lens.Lens' BaiduChannelResponse (Core.Maybe Core.Text)
bcrLastModifiedBy = Lens.field @"lastModifiedBy"
{-# INLINEABLE bcrLastModifiedBy #-}
{-# DEPRECATED lastModifiedBy "Use generic-lens or generic-optics with 'lastModifiedBy' instead"  #-}

-- | The date and time when the Baidu channel was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrLastModifiedDate :: Lens.Lens' BaiduChannelResponse (Core.Maybe Core.Text)
bcrLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE bcrLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

-- | The current version of the Baidu channel.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcrVersion :: Lens.Lens' BaiduChannelResponse (Core.Maybe Core.Int)
bcrVersion = Lens.field @"version"
{-# INLINEABLE bcrVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON BaiduChannelResponse where
        parseJSON
          = Core.withObject "BaiduChannelResponse" Core.$
              \ x ->
                BaiduChannelResponse' Core.<$>
                  (x Core..: "Credential") Core.<*> x Core..: "Platform" Core.<*>
                    x Core..:? "ApplicationId"
                    Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "Enabled"
                    Core.<*> x Core..:? "HasCredential"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "IsArchived"
                    Core.<*> x Core..:? "LastModifiedBy"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "Version"
