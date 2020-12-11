-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.APIKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.APIKey
  ( APIKey (..),

    -- * Smart constructor
    mkAPIKey,

    -- * Lenses
    akExpires,
    akDeletes,
    akId,
    akDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
-- /See:/ 'mkAPIKey' smart constructor.
data APIKey = APIKey'
  { expires :: Lude.Maybe Lude.Integer,
    deletes :: Lude.Maybe Lude.Integer,
    id :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APIKey' with the minimum fields required to make a request.
--
-- * 'deletes' - The time after which the API key is deleted. The date is represented as seconds since the epoch, rounded down to the nearest hour.
-- * 'description' - A description of the purpose of the API key.
-- * 'expires' - The time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour.
-- * 'id' - The API key ID.
mkAPIKey ::
  APIKey
mkAPIKey =
  APIKey'
    { expires = Lude.Nothing,
      deletes = Lude.Nothing,
      id = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour.
--
-- /Note:/ Consider using 'expires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akExpires :: Lens.Lens' APIKey (Lude.Maybe Lude.Integer)
akExpires = Lens.lens (expires :: APIKey -> Lude.Maybe Lude.Integer) (\s a -> s {expires = a} :: APIKey)
{-# DEPRECATED akExpires "Use generic-lens or generic-optics with 'expires' instead." #-}

-- | The time after which the API key is deleted. The date is represented as seconds since the epoch, rounded down to the nearest hour.
--
-- /Note:/ Consider using 'deletes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akDeletes :: Lens.Lens' APIKey (Lude.Maybe Lude.Integer)
akDeletes = Lens.lens (deletes :: APIKey -> Lude.Maybe Lude.Integer) (\s a -> s {deletes = a} :: APIKey)
{-# DEPRECATED akDeletes "Use generic-lens or generic-optics with 'deletes' instead." #-}

-- | The API key ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akId :: Lens.Lens' APIKey (Lude.Maybe Lude.Text)
akId = Lens.lens (id :: APIKey -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: APIKey)
{-# DEPRECATED akId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A description of the purpose of the API key.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akDescription :: Lens.Lens' APIKey (Lude.Maybe Lude.Text)
akDescription = Lens.lens (description :: APIKey -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: APIKey)
{-# DEPRECATED akDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON APIKey where
  parseJSON =
    Lude.withObject
      "APIKey"
      ( \x ->
          APIKey'
            Lude.<$> (x Lude..:? "expires")
            Lude.<*> (x Lude..:? "deletes")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "description")
      )
