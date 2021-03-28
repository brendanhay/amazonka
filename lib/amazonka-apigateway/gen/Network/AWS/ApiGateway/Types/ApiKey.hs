{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.ApiKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.ApiKey
  ( ApiKey (..)
  -- * Smart constructor
  , mkApiKey
  -- * Lenses
  , akCreatedDate
  , akCustomerId
  , akDescription
  , akEnabled
  , akId
  , akLastUpdatedDate
  , akName
  , akStageKeys
  , akTags
  , akValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A resource that can be distributed to callers for executing 'Method' resources that require an API key. API keys can be mapped to any 'Stage' on any 'RestApi' , which indicates that the callers with the API key can make requests to that stage.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-api-keys.html Use API Keys> 
--
-- /See:/ 'mkApiKey' smart constructor.
data ApiKey = ApiKey'
  { createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the API Key was created.
  , customerId :: Core.Maybe Core.Text
    -- ^ An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the API Key.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the API Key can be used by callers.
  , id :: Core.Maybe Core.Text
    -- ^ The identifier of the API Key.
  , lastUpdatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the API Key was last updated.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the API Key.
  , stageKeys :: Core.Maybe [Core.Text]
    -- ^ A list of 'Stage' resources that are associated with the 'ApiKey' resource.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The collection of tags. Each tag element is associated with a given resource.
  , value :: Core.Maybe Core.Text
    -- ^ The value of the API Key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ApiKey' value with any optional fields omitted.
mkApiKey
    :: ApiKey
mkApiKey
  = ApiKey'{createdDate = Core.Nothing, customerId = Core.Nothing,
            description = Core.Nothing, enabled = Core.Nothing,
            id = Core.Nothing, lastUpdatedDate = Core.Nothing,
            name = Core.Nothing, stageKeys = Core.Nothing, tags = Core.Nothing,
            value = Core.Nothing}

-- | The timestamp when the API Key was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akCreatedDate :: Lens.Lens' ApiKey (Core.Maybe Core.NominalDiffTime)
akCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE akCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
--
-- /Note:/ Consider using 'customerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akCustomerId :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
akCustomerId = Lens.field @"customerId"
{-# INLINEABLE akCustomerId #-}
{-# DEPRECATED customerId "Use generic-lens or generic-optics with 'customerId' instead"  #-}

-- | The description of the API Key.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akDescription :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
akDescription = Lens.field @"description"
{-# INLINEABLE akDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies whether the API Key can be used by callers.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akEnabled :: Lens.Lens' ApiKey (Core.Maybe Core.Bool)
akEnabled = Lens.field @"enabled"
{-# INLINEABLE akEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The identifier of the API Key.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akId :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
akId = Lens.field @"id"
{-# INLINEABLE akId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The timestamp when the API Key was last updated.
--
-- /Note:/ Consider using 'lastUpdatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akLastUpdatedDate :: Lens.Lens' ApiKey (Core.Maybe Core.NominalDiffTime)
akLastUpdatedDate = Lens.field @"lastUpdatedDate"
{-# INLINEABLE akLastUpdatedDate #-}
{-# DEPRECATED lastUpdatedDate "Use generic-lens or generic-optics with 'lastUpdatedDate' instead"  #-}

-- | The name of the API Key.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akName :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
akName = Lens.field @"name"
{-# INLINEABLE akName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of 'Stage' resources that are associated with the 'ApiKey' resource.
--
-- /Note:/ Consider using 'stageKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akStageKeys :: Lens.Lens' ApiKey (Core.Maybe [Core.Text])
akStageKeys = Lens.field @"stageKeys"
{-# INLINEABLE akStageKeys #-}
{-# DEPRECATED stageKeys "Use generic-lens or generic-optics with 'stageKeys' instead"  #-}

-- | The collection of tags. Each tag element is associated with a given resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akTags :: Lens.Lens' ApiKey (Core.Maybe (Core.HashMap Core.Text Core.Text))
akTags = Lens.field @"tags"
{-# INLINEABLE akTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The value of the API Key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akValue :: Lens.Lens' ApiKey (Core.Maybe Core.Text)
akValue = Lens.field @"value"
{-# INLINEABLE akValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.FromJSON ApiKey where
        parseJSON
          = Core.withObject "ApiKey" Core.$
              \ x ->
                ApiKey' Core.<$>
                  (x Core..:? "createdDate") Core.<*> x Core..:? "customerId"
                    Core.<*> x Core..:? "description"
                    Core.<*> x Core..:? "enabled"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "lastUpdatedDate"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "stageKeys"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "value"
