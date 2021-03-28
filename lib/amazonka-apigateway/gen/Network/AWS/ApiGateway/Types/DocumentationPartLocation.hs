{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.DocumentationPartLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.DocumentationPartLocation
  ( DocumentationPartLocation (..)
  -- * Smart constructor
  , mkDocumentationPartLocation
  -- * Lenses
  , dplType
  , dplMethod
  , dplName
  , dplPath
  , dplStatusCode
  ) where

import qualified Network.AWS.ApiGateway.Types.DocumentationPartLocationStatusCode as Types
import qualified Network.AWS.ApiGateway.Types.DocumentationPartType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the target API entity to which the documentation applies.
--
-- /See:/ 'mkDocumentationPartLocation' smart constructor.
data DocumentationPartLocation = DocumentationPartLocation'
  { type' :: Types.DocumentationPartType
    -- ^ [Required] The type of API entity to which the documentation content applies. Valid values are @API@ , @AUTHORIZER@ , @MODEL@ , @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . Content inheritance does not apply to any entity of the @API@ , @AUTHORIZER@ , @METHOD@ , @MODEL@ , @REQUEST_BODY@ , or @RESOURCE@ type.
  , method :: Core.Maybe Core.Text
    -- ^ The HTTP verb of a method. It is a valid field for the API entity types of @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any method. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @method@ attribute must match that of the parent entity exactly.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the targeted API entity. It is a valid and required field for the API entity types of @AUTHORIZER@ , @MODEL@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ and @RESPONSE_HEADER@ . It is an invalid field for any other entity type.
  , path :: Core.Maybe Core.Text
    -- ^ The URL path of the target. It is a valid field for the API entity types of @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @/@ for the root resource. When an applicable child entity inherits the content of another entity of the same type with more general specifications of the other @location@ attributes, the child entity's @path@ attribute must match that of the parent entity as a prefix.
  , statusCode :: Core.Maybe Types.DocumentationPartLocationStatusCode
    -- ^ The HTTP status code of a response. It is a valid field for the API entity types of @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any status code. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @statusCode@ attribute must match that of the parent entity exactly.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentationPartLocation' value with any optional fields omitted.
mkDocumentationPartLocation
    :: Types.DocumentationPartType -- ^ 'type\''
    -> DocumentationPartLocation
mkDocumentationPartLocation type'
  = DocumentationPartLocation'{type', method = Core.Nothing,
                               name = Core.Nothing, path = Core.Nothing,
                               statusCode = Core.Nothing}

-- | [Required] The type of API entity to which the documentation content applies. Valid values are @API@ , @AUTHORIZER@ , @MODEL@ , @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . Content inheritance does not apply to any entity of the @API@ , @AUTHORIZER@ , @METHOD@ , @MODEL@ , @REQUEST_BODY@ , or @RESOURCE@ type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplType :: Lens.Lens' DocumentationPartLocation Types.DocumentationPartType
dplType = Lens.field @"type'"
{-# INLINEABLE dplType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The HTTP verb of a method. It is a valid field for the API entity types of @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any method. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @method@ attribute must match that of the parent entity exactly.
--
-- /Note:/ Consider using 'method' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplMethod :: Lens.Lens' DocumentationPartLocation (Core.Maybe Core.Text)
dplMethod = Lens.field @"method"
{-# INLINEABLE dplMethod #-}
{-# DEPRECATED method "Use generic-lens or generic-optics with 'method' instead"  #-}

-- | The name of the targeted API entity. It is a valid and required field for the API entity types of @AUTHORIZER@ , @MODEL@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ and @RESPONSE_HEADER@ . It is an invalid field for any other entity type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplName :: Lens.Lens' DocumentationPartLocation (Core.Maybe Core.Text)
dplName = Lens.field @"name"
{-# INLINEABLE dplName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The URL path of the target. It is a valid field for the API entity types of @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @/@ for the root resource. When an applicable child entity inherits the content of another entity of the same type with more general specifications of the other @location@ attributes, the child entity's @path@ attribute must match that of the parent entity as a prefix.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplPath :: Lens.Lens' DocumentationPartLocation (Core.Maybe Core.Text)
dplPath = Lens.field @"path"
{-# INLINEABLE dplPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

-- | The HTTP status code of a response. It is a valid field for the API entity types of @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any status code. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @statusCode@ attribute must match that of the parent entity exactly.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplStatusCode :: Lens.Lens' DocumentationPartLocation (Core.Maybe Types.DocumentationPartLocationStatusCode)
dplStatusCode = Lens.field @"statusCode"
{-# INLINEABLE dplStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

instance Core.FromJSON DocumentationPartLocation where
        toJSON DocumentationPartLocation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("type" Core..= type'),
                  ("method" Core..=) Core.<$> method, ("name" Core..=) Core.<$> name,
                  ("path" Core..=) Core.<$> path,
                  ("statusCode" Core..=) Core.<$> statusCode])

instance Core.FromJSON DocumentationPartLocation where
        parseJSON
          = Core.withObject "DocumentationPartLocation" Core.$
              \ x ->
                DocumentationPartLocation' Core.<$>
                  (x Core..: "type") Core.<*> x Core..:? "method" Core.<*>
                    x Core..:? "name"
                    Core.<*> x Core..:? "path"
                    Core.<*> x Core..:? "statusCode"
