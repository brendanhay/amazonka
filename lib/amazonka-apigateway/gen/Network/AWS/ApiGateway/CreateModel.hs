{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.CreateModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new 'Model' resource to an existing 'RestApi' resource.
module Network.AWS.ApiGateway.CreateModel
    (
    -- * Creating a request
      CreateModel (..)
    , mkCreateModel
    -- ** Request lenses
    , cmRestApiId
    , cmName
    , cmContentType
    , cmDescription
    , cmSchema

     -- * Destructuring the response
    , Types.Model (..)
    , Types.mkModel
    -- ** Response lenses
    , Types.mContentType
    , Types.mDescription
    , Types.mId
    , Types.mName
    , Types.mSchema
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to add a new 'Model' to an existing 'RestApi' resource.
--
-- /See:/ 'mkCreateModel' smart constructor.
data CreateModel = CreateModel'
  { restApiId :: Core.Text
    -- ^ [Required] The 'RestApi' identifier under which the 'Model' will be created.
  , name :: Core.Text
    -- ^ [Required] The name of the model. Must be alphanumeric.
  , contentType :: Core.Text
    -- ^ [Required] The content-type for the model.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the model.
  , schema :: Core.Maybe Core.Text
    -- ^ The schema for the model. For @application/json@ models, this should be <https://tools.ietf.org/html/draft-zyp-json-schema-04 JSON schema draft 4> model.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateModel' value with any optional fields omitted.
mkCreateModel
    :: Core.Text -- ^ 'restApiId'
    -> Core.Text -- ^ 'name'
    -> Core.Text -- ^ 'contentType'
    -> CreateModel
mkCreateModel restApiId name contentType
  = CreateModel'{restApiId, name, contentType,
                 description = Core.Nothing, schema = Core.Nothing}

-- | [Required] The 'RestApi' identifier under which the 'Model' will be created.
--
-- /Note:/ Consider using 'restApiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmRestApiId :: Lens.Lens' CreateModel Core.Text
cmRestApiId = Lens.field @"restApiId"
{-# INLINEABLE cmRestApiId #-}
{-# DEPRECATED restApiId "Use generic-lens or generic-optics with 'restApiId' instead"  #-}

-- | [Required] The name of the model. Must be alphanumeric.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmName :: Lens.Lens' CreateModel Core.Text
cmName = Lens.field @"name"
{-# INLINEABLE cmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | [Required] The content-type for the model.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmContentType :: Lens.Lens' CreateModel Core.Text
cmContentType = Lens.field @"contentType"
{-# INLINEABLE cmContentType #-}
{-# DEPRECATED contentType "Use generic-lens or generic-optics with 'contentType' instead"  #-}

-- | The description of the model.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmDescription :: Lens.Lens' CreateModel (Core.Maybe Core.Text)
cmDescription = Lens.field @"description"
{-# INLINEABLE cmDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The schema for the model. For @application/json@ models, this should be <https://tools.ietf.org/html/draft-zyp-json-schema-04 JSON schema draft 4> model.
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmSchema :: Lens.Lens' CreateModel (Core.Maybe Core.Text)
cmSchema = Lens.field @"schema"
{-# INLINEABLE cmSchema #-}
{-# DEPRECATED schema "Use generic-lens or generic-optics with 'schema' instead"  #-}

instance Core.ToQuery CreateModel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateModel where
        toHeaders CreateModel{..}
          = Core.pure ("Accept", "application/json")

instance Core.FromJSON CreateModel where
        toJSON CreateModel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("name" Core..= name),
                  Core.Just ("contentType" Core..= contentType),
                  ("description" Core..=) Core.<$> description,
                  ("schema" Core..=) Core.<$> schema])

instance Core.AWSRequest CreateModel where
        type Rs CreateModel = Types.Model
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/restapis/" Core.<> Core.toText restApiId Core.<> "/models",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
