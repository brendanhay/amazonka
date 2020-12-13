{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.APIKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.APIKey
  ( APIKey (..),

    -- * Smart constructor
    mkAPIKey,

    -- * Lenses
    akEnabled,
    akValue,
    akCustomerId,
    akCreatedDate,
    akName,
    akId,
    akStageKeys,
    akLastUpdatedDate,
    akDescription,
    akTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A resource that can be distributed to callers for executing 'Method' resources that require an API key. API keys can be mapped to any 'Stage' on any 'RestApi' , which indicates that the callers with the API key can make requests to that stage.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html Use API Keys>
--
-- /See:/ 'mkAPIKey' smart constructor.
data APIKey = APIKey'
  { -- | Specifies whether the API Key can be used by callers.
    enabled :: Lude.Maybe Lude.Bool,
    -- | The value of the API Key.
    value :: Lude.Maybe Lude.Text,
    -- | An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
    customerId :: Lude.Maybe Lude.Text,
    -- | The timestamp when the API Key was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the API Key.
    name :: Lude.Maybe Lude.Text,
    -- | The identifier of the API Key.
    id :: Lude.Maybe Lude.Text,
    -- | A list of 'Stage' resources that are associated with the 'ApiKey' resource.
    stageKeys :: Lude.Maybe [Lude.Text],
    -- | The timestamp when the API Key was last updated.
    lastUpdatedDate :: Lude.Maybe Lude.Timestamp,
    -- | The description of the API Key.
    description :: Lude.Maybe Lude.Text,
    -- | The collection of tags. Each tag element is associated with a given resource.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APIKey' with the minimum fields required to make a request.
--
-- * 'enabled' - Specifies whether the API Key can be used by callers.
-- * 'value' - The value of the API Key.
-- * 'customerId' - An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
-- * 'createdDate' - The timestamp when the API Key was created.
-- * 'name' - The name of the API Key.
-- * 'id' - The identifier of the API Key.
-- * 'stageKeys' - A list of 'Stage' resources that are associated with the 'ApiKey' resource.
-- * 'lastUpdatedDate' - The timestamp when the API Key was last updated.
-- * 'description' - The description of the API Key.
-- * 'tags' - The collection of tags. Each tag element is associated with a given resource.
mkAPIKey ::
  APIKey
mkAPIKey =
  APIKey'
    { enabled = Lude.Nothing,
      value = Lude.Nothing,
      customerId = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      stageKeys = Lude.Nothing,
      lastUpdatedDate = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Specifies whether the API Key can be used by callers.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akEnabled :: Lens.Lens' APIKey (Lude.Maybe Lude.Bool)
akEnabled = Lens.lens (enabled :: APIKey -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: APIKey)
{-# DEPRECATED akEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The value of the API Key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akValue :: Lens.Lens' APIKey (Lude.Maybe Lude.Text)
akValue = Lens.lens (value :: APIKey -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: APIKey)
{-# DEPRECATED akValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
--
-- /Note:/ Consider using 'customerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akCustomerId :: Lens.Lens' APIKey (Lude.Maybe Lude.Text)
akCustomerId = Lens.lens (customerId :: APIKey -> Lude.Maybe Lude.Text) (\s a -> s {customerId = a} :: APIKey)
{-# DEPRECATED akCustomerId "Use generic-lens or generic-optics with 'customerId' instead." #-}

-- | The timestamp when the API Key was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akCreatedDate :: Lens.Lens' APIKey (Lude.Maybe Lude.Timestamp)
akCreatedDate = Lens.lens (createdDate :: APIKey -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: APIKey)
{-# DEPRECATED akCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the API Key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akName :: Lens.Lens' APIKey (Lude.Maybe Lude.Text)
akName = Lens.lens (name :: APIKey -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: APIKey)
{-# DEPRECATED akName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the API Key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akId :: Lens.Lens' APIKey (Lude.Maybe Lude.Text)
akId = Lens.lens (id :: APIKey -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: APIKey)
{-# DEPRECATED akId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | A list of 'Stage' resources that are associated with the 'ApiKey' resource.
--
-- /Note:/ Consider using 'stageKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akStageKeys :: Lens.Lens' APIKey (Lude.Maybe [Lude.Text])
akStageKeys = Lens.lens (stageKeys :: APIKey -> Lude.Maybe [Lude.Text]) (\s a -> s {stageKeys = a} :: APIKey)
{-# DEPRECATED akStageKeys "Use generic-lens or generic-optics with 'stageKeys' instead." #-}

-- | The timestamp when the API Key was last updated.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akLastUpdatedDate :: Lens.Lens' APIKey (Lude.Maybe Lude.Timestamp)
akLastUpdatedDate = Lens.lens (lastUpdatedDate :: APIKey -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastUpdatedDate = a} :: APIKey)
{-# DEPRECATED akLastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead." #-}

-- | The description of the API Key.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akDescription :: Lens.Lens' APIKey (Lude.Maybe Lude.Text)
akDescription = Lens.lens (description :: APIKey -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: APIKey)
{-# DEPRECATED akDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akTags :: Lens.Lens' APIKey (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
akTags = Lens.lens (tags :: APIKey -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: APIKey)
{-# DEPRECATED akTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON APIKey where
  parseJSON =
    Lude.withObject
      "APIKey"
      ( \x ->
          APIKey'
            Lude.<$> (x Lude..:? "enabled")
            Lude.<*> (x Lude..:? "value")
            Lude.<*> (x Lude..:? "customerId")
            Lude.<*> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "stageKeys" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "lastUpdatedDate")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
