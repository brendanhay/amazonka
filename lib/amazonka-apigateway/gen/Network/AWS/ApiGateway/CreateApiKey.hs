{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateApiKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an 'ApiKey' resource. 
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/create-api-key.html AWS CLI> 
module Network.AWS.ApiGateway.CreateApiKey
    (
    -- * Creating a request
      CreateApiKey (..)
    , mkCreateApiKey
    -- ** Request lenses
    , cakCustomerId
    , cakDescription
    , cakEnabled
    , cakGenerateDistinctId
    , cakName
    , cakStageKeys
    , cakTags
    , cakValue

     -- * Destructuring the response
    , Types.ApiKey (..)
    , Types.mkApiKey
    -- ** Response lenses
    , Types.akCreatedDate
    , Types.akCustomerId
    , Types.akDescription
    , Types.akEnabled
    , Types.akId
    , Types.akLastUpdatedDate
    , Types.akName
    , Types.akStageKeys
    , Types.akTags
    , Types.akValue
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to create an 'ApiKey' resource.
--
-- /See:/ 'mkCreateApiKey' smart constructor.
data CreateApiKey = CreateApiKey'
  { customerId :: Core.Maybe Core.Text
    -- ^ An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the 'ApiKey' .
  , enabled :: Core.Maybe Core.Bool
    -- ^ Specifies whether the 'ApiKey' can be used by callers.
  , generateDistinctId :: Core.Maybe Core.Bool
    -- ^ Specifies whether (@true@ ) or not (@false@ ) the key identifier is distinct from the created API key value. This parameter is deprecated and should not be used.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the 'ApiKey' .
  , stageKeys :: Core.Maybe [Types.StageKey]
    -- ^ DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API key.
  , tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
  , value :: Core.Maybe Core.Text
    -- ^ Specifies a value of the API key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApiKey' value with any optional fields omitted.
mkCreateApiKey
    :: CreateApiKey
mkCreateApiKey
  = CreateApiKey'{customerId = Core.Nothing,
                  description = Core.Nothing, enabled = Core.Nothing,
                  generateDistinctId = Core.Nothing, name = Core.Nothing,
                  stageKeys = Core.Nothing, tags = Core.Nothing,
                  value = Core.Nothing}

-- | An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
--
-- /Note:/ Consider using 'customerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakCustomerId :: Lens.Lens' CreateApiKey (Core.Maybe Core.Text)
cakCustomerId = Lens.field @"customerId"
{-# INLINEABLE cakCustomerId #-}
{-# DEPRECATED customerId "Use generic-lens or generic-optics with 'customerId' instead"  #-}

-- | The description of the 'ApiKey' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakDescription :: Lens.Lens' CreateApiKey (Core.Maybe Core.Text)
cakDescription = Lens.field @"description"
{-# INLINEABLE cakDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies whether the 'ApiKey' can be used by callers.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakEnabled :: Lens.Lens' CreateApiKey (Core.Maybe Core.Bool)
cakEnabled = Lens.field @"enabled"
{-# INLINEABLE cakEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | Specifies whether (@true@ ) or not (@false@ ) the key identifier is distinct from the created API key value. This parameter is deprecated and should not be used.
--
-- /Note:/ Consider using 'generateDistinctId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakGenerateDistinctId :: Lens.Lens' CreateApiKey (Core.Maybe Core.Bool)
cakGenerateDistinctId = Lens.field @"generateDistinctId"
{-# INLINEABLE cakGenerateDistinctId #-}
{-# DEPRECATED generateDistinctId "Use generic-lens or generic-optics with 'generateDistinctId' instead"  #-}

-- | The name of the 'ApiKey' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakName :: Lens.Lens' CreateApiKey (Core.Maybe Core.Text)
cakName = Lens.field @"name"
{-# INLINEABLE cakName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API key.
--
-- /Note:/ Consider using 'stageKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakStageKeys :: Lens.Lens' CreateApiKey (Core.Maybe [Types.StageKey])
cakStageKeys = Lens.field @"stageKeys"
{-# INLINEABLE cakStageKeys #-}
{-# DEPRECATED stageKeys "Use generic-lens or generic-optics with 'stageKeys' instead"  #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakTags :: Lens.Lens' CreateApiKey (Core.Maybe (Core.HashMap Core.Text Core.Text))
cakTags = Lens.field @"tags"
{-# INLINEABLE cakTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | Specifies a value of the API key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakValue :: Lens.Lens' CreateApiKey (Core.Maybe Core.Text)
cakValue = Lens.field @"value"
{-# INLINEABLE cakValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery CreateApiKey where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateApiKey where
        toHeaders CreateApiKey{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateApiKey where
        toJSON CreateApiKey{..}
          = Core.object
              (Core.catMaybes
                 [("customerId" Core..=) Core.<$> customerId,
                  ("description" Core..=) Core.<$> description,
                  ("enabled" Core..=) Core.<$> enabled,
                  ("generateDistinctId" Core..=) Core.<$> generateDistinctId,
                  ("name" Core..=) Core.<$> name,
                  ("stageKeys" Core..=) Core.<$> stageKeys,
                  ("tags" Core..=) Core.<$> tags, ("value" Core..=) Core.<$> value])

instance Core.AWSRequest CreateApiKey where
        type Rs CreateApiKey = Types.ApiKey
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/apikeys",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
