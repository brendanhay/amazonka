{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ApiKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.ApiKey
  ( ApiKey (..)
  -- * Smart constructor
  , mkApiKey
  -- * Lenses
  , akDeletes
  , akDescription
  , akExpires
  , akId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an API key.
--
-- Customers invoke AWS AppSync GraphQL API operations with API keys as an identity mechanism. There are two key versions:
-- __da1__ : This version was introduced at launch in November 2017. These keys always expire after 7 days. Key expiration is managed by Amazon DynamoDB TTL. The keys ceased to be valid after February 21, 2018 and should not be used after that date.
--
--     * @ListApiKeys@ returns the expiration time in milliseconds.
--
--
--     * @CreateApiKey@ returns the expiration time in milliseconds.
--
--
--     * @UpdateApiKey@ is not available for this key version.
--
--
--     * @DeleteApiKey@ deletes the item from the table.
--
--
--     * Expiration is stored in Amazon DynamoDB as milliseconds. This results in a bug where keys are not automatically deleted because DynamoDB expects the TTL to be stored in seconds. As a one-time action, we will delete these keys from the table after February 21, 2018.
--
--
-- __da2__ : This version was introduced in February 2018 when AppSync added support to extend key expiration.
--
--     * @ListApiKeys@ returns the expiration time and deletion time in seconds.
--
--
--     * @CreateApiKey@ returns the expiration time and deletion time in seconds and accepts a user-provided expiration time in seconds.
--
--
--     * @UpdateApiKey@ returns the expiration time and and deletion time in seconds and accepts a user-provided expiration time in seconds. Expired API keys are kept for 60 days after the expiration time. Key expiration time can be updated while the key is not deleted. 
--
--
--     * @DeleteApiKey@ deletes the item from the table.
--
--
--     * Expiration is stored in Amazon DynamoDB as seconds. After the expiration time, using the key to authenticate will fail. But the key can be reinstated before deletion.
--
--
--     * Deletion is stored in Amazon DynamoDB as seconds. The key will be deleted after deletion time. 
--
--
--
-- /See:/ 'mkApiKey' smart constructor.
data ApiKey = ApiKey'
  { deletes :: Core.Maybe Core.Integer
    -- ^ The time after which the API key is deleted. The date is represented as seconds since the epoch, rounded down to the nearest hour.
  , description :: Core.Maybe Core.Text
    -- ^ A description of the purpose of the API key.
  , expires :: Core.Maybe Core.Integer
    -- ^ The time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour.
  , id :: Core.Maybe Core.Text
    -- ^ The API key ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApiKey' value with any optional fields omitted.
mkApiKey
    :: ApiKey
mkApiKey
  = ApiKey'{deletes = Core.Nothing, description = Core.Nothing,
            expires = Core.Nothing, id = Core.Nothing}

-- | The time after which the API key is deleted. The date is represented as seconds since the epoch, rounded down to the nearest hour.
--
-- /Note:/ Consider using 'deletes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akDeletes :: Lens.Lens' ApiKey (Core.Maybe Core.Integer)
akDeletes = Lens.field @"deletes"
{-# INLINEABLE akDeletes #-}
{-# DEPRECATED deletes "Use generic-lens or generic-optics with 'deletes' instead"  #-}

-- | A description of the purpose of the API key.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akDescription :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
akDescription = Lens.field @"description"
{-# INLINEABLE akDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akExpires :: Lens.Lens' ApiKey (Core.Maybe Core.Integer)
akExpires = Lens.field @"expires"
{-# INLINEABLE akExpires #-}
{-# DEPRECATED expires "Use generic-lens or generic-optics with 'expires' instead"  #-}

-- | The API key ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akId :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
akId = Lens.field @"id"
{-# INLINEABLE akId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON ApiKey where
        parseJSON
          = Core.withObject "ApiKey" Core.$
              \ x ->
                ApiKey' Core.<$>
                  (x Core..:? "deletes") Core.<*> x Core..:? "description" Core.<*>
                    x Core..:? "expires"
                    Core.<*> x Core..:? "id"
