{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DocumentationPartLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DocumentationPartLocation
  ( DocumentationPartLocation (..),

    -- * Smart constructor
    mkDocumentationPartLocation,

    -- * Lenses
    dplPath,
    dplName,
    dplMethod,
    dplStatusCode,
    dplType,
  )
where

import Network.AWS.APIGateway.Types.DocumentationPartType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the target API entity to which the documentation applies.
--
-- /See:/ 'mkDocumentationPartLocation' smart constructor.
data DocumentationPartLocation = DocumentationPartLocation'
  { path ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    method :: Lude.Maybe Lude.Text,
    statusCode :: Lude.Maybe Lude.Text,
    type' :: DocumentationPartType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentationPartLocation' with the minimum fields required to make a request.
--
-- * 'method' - The HTTP verb of a method. It is a valid field for the API entity types of @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any method. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @method@ attribute must match that of the parent entity exactly.
-- * 'name' - The name of the targeted API entity. It is a valid and required field for the API entity types of @AUTHORIZER@ , @MODEL@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ and @RESPONSE_HEADER@ . It is an invalid field for any other entity type.
-- * 'path' - The URL path of the target. It is a valid field for the API entity types of @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @/@ for the root resource. When an applicable child entity inherits the content of another entity of the same type with more general specifications of the other @location@ attributes, the child entity's @path@ attribute must match that of the parent entity as a prefix.
-- * 'statusCode' - The HTTP status code of a response. It is a valid field for the API entity types of @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any status code. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @statusCode@ attribute must match that of the parent entity exactly.
-- * 'type'' - [Required] The type of API entity to which the documentation content applies. Valid values are @API@ , @AUTHORIZER@ , @MODEL@ , @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . Content inheritance does not apply to any entity of the @API@ , @AUTHORIZER@ , @METHOD@ , @MODEL@ , @REQUEST_BODY@ , or @RESOURCE@ type.
mkDocumentationPartLocation ::
  -- | 'type''
  DocumentationPartType ->
  DocumentationPartLocation
mkDocumentationPartLocation pType_ =
  DocumentationPartLocation'
    { path = Lude.Nothing,
      name = Lude.Nothing,
      method = Lude.Nothing,
      statusCode = Lude.Nothing,
      type' = pType_
    }

-- | The URL path of the target. It is a valid field for the API entity types of @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @/@ for the root resource. When an applicable child entity inherits the content of another entity of the same type with more general specifications of the other @location@ attributes, the child entity's @path@ attribute must match that of the parent entity as a prefix.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplPath :: Lens.Lens' DocumentationPartLocation (Lude.Maybe Lude.Text)
dplPath = Lens.lens (path :: DocumentationPartLocation -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: DocumentationPartLocation)
{-# DEPRECATED dplPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The name of the targeted API entity. It is a valid and required field for the API entity types of @AUTHORIZER@ , @MODEL@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ and @RESPONSE_HEADER@ . It is an invalid field for any other entity type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplName :: Lens.Lens' DocumentationPartLocation (Lude.Maybe Lude.Text)
dplName = Lens.lens (name :: DocumentationPartLocation -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DocumentationPartLocation)
{-# DEPRECATED dplName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The HTTP verb of a method. It is a valid field for the API entity types of @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any method. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @method@ attribute must match that of the parent entity exactly.
--
-- /Note:/ Consider using 'method' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplMethod :: Lens.Lens' DocumentationPartLocation (Lude.Maybe Lude.Text)
dplMethod = Lens.lens (method :: DocumentationPartLocation -> Lude.Maybe Lude.Text) (\s a -> s {method = a} :: DocumentationPartLocation)
{-# DEPRECATED dplMethod "Use generic-lens or generic-optics with 'method' instead." #-}

-- | The HTTP status code of a response. It is a valid field for the API entity types of @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any status code. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @statusCode@ attribute must match that of the parent entity exactly.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplStatusCode :: Lens.Lens' DocumentationPartLocation (Lude.Maybe Lude.Text)
dplStatusCode = Lens.lens (statusCode :: DocumentationPartLocation -> Lude.Maybe Lude.Text) (\s a -> s {statusCode = a} :: DocumentationPartLocation)
{-# DEPRECATED dplStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

-- | [Required] The type of API entity to which the documentation content applies. Valid values are @API@ , @AUTHORIZER@ , @MODEL@ , @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . Content inheritance does not apply to any entity of the @API@ , @AUTHORIZER@ , @METHOD@ , @MODEL@ , @REQUEST_BODY@ , or @RESOURCE@ type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dplType :: Lens.Lens' DocumentationPartLocation DocumentationPartType
dplType = Lens.lens (type' :: DocumentationPartLocation -> DocumentationPartType) (\s a -> s {type' = a} :: DocumentationPartLocation)
{-# DEPRECATED dplType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON DocumentationPartLocation where
  parseJSON =
    Lude.withObject
      "DocumentationPartLocation"
      ( \x ->
          DocumentationPartLocation'
            Lude.<$> (x Lude..:? "path")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "method")
            Lude.<*> (x Lude..:? "statusCode")
            Lude.<*> (x Lude..: "type")
      )

instance Lude.ToJSON DocumentationPartLocation where
  toJSON DocumentationPartLocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("path" Lude..=) Lude.<$> path,
            ("name" Lude..=) Lude.<$> name,
            ("method" Lude..=) Lude.<$> method,
            ("statusCode" Lude..=) Lude.<$> statusCode,
            Lude.Just ("type" Lude..= type')
          ]
      )
