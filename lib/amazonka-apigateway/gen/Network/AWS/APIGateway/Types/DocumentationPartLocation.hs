{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DocumentationPartLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DocumentationPartLocation where

import Network.AWS.APIGateway.Types.DocumentationPartType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the target API entity to which the documentation applies.
--
--
--
-- /See:/ 'documentationPartLocation' smart constructor.
data DocumentationPartLocation = DocumentationPartLocation'
  { _dplPath ::
      !(Maybe Text),
    _dplName :: !(Maybe Text),
    _dplMethod :: !(Maybe Text),
    _dplStatusCode :: !(Maybe Text),
    _dplType :: !DocumentationPartType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentationPartLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dplPath' - The URL path of the target. It is a valid field for the API entity types of @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @/@ for the root resource. When an applicable child entity inherits the content of another entity of the same type with more general specifications of the other @location@ attributes, the child entity's @path@ attribute must match that of the parent entity as a prefix.
--
-- * 'dplName' - The name of the targeted API entity. It is a valid and required field for the API entity types of @AUTHORIZER@ , @MODEL@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ and @RESPONSE_HEADER@ . It is an invalid field for any other entity type.
--
-- * 'dplMethod' - The HTTP verb of a method. It is a valid field for the API entity types of @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any method. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @method@ attribute must match that of the parent entity exactly.
--
-- * 'dplStatusCode' - The HTTP status code of a response. It is a valid field for the API entity types of @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any status code. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @statusCode@ attribute must match that of the parent entity exactly.
--
-- * 'dplType' - [Required] The type of API entity to which the documentation content applies. Valid values are @API@ , @AUTHORIZER@ , @MODEL@ , @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . Content inheritance does not apply to any entity of the @API@ , @AUTHORIZER@ , @METHOD@ , @MODEL@ , @REQUEST_BODY@ , or @RESOURCE@ type.
documentationPartLocation ::
  -- | 'dplType'
  DocumentationPartType ->
  DocumentationPartLocation
documentationPartLocation pType_ =
  DocumentationPartLocation'
    { _dplPath = Nothing,
      _dplName = Nothing,
      _dplMethod = Nothing,
      _dplStatusCode = Nothing,
      _dplType = pType_
    }

-- | The URL path of the target. It is a valid field for the API entity types of @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @/@ for the root resource. When an applicable child entity inherits the content of another entity of the same type with more general specifications of the other @location@ attributes, the child entity's @path@ attribute must match that of the parent entity as a prefix.
dplPath :: Lens' DocumentationPartLocation (Maybe Text)
dplPath = lens _dplPath (\s a -> s {_dplPath = a})

-- | The name of the targeted API entity. It is a valid and required field for the API entity types of @AUTHORIZER@ , @MODEL@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ and @RESPONSE_HEADER@ . It is an invalid field for any other entity type.
dplName :: Lens' DocumentationPartLocation (Maybe Text)
dplName = lens _dplName (\s a -> s {_dplName = a})

-- | The HTTP verb of a method. It is a valid field for the API entity types of @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any method. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @method@ attribute must match that of the parent entity exactly.
dplMethod :: Lens' DocumentationPartLocation (Maybe Text)
dplMethod = lens _dplMethod (\s a -> s {_dplMethod = a})

-- | The HTTP status code of a response. It is a valid field for the API entity types of @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . The default value is @*@ for any status code. When an applicable child entity inherits the content of an entity of the same type with more general specifications of the other @location@ attributes, the child entity's @statusCode@ attribute must match that of the parent entity exactly.
dplStatusCode :: Lens' DocumentationPartLocation (Maybe Text)
dplStatusCode = lens _dplStatusCode (\s a -> s {_dplStatusCode = a})

-- | [Required] The type of API entity to which the documentation content applies. Valid values are @API@ , @AUTHORIZER@ , @MODEL@ , @RESOURCE@ , @METHOD@ , @PATH_PARAMETER@ , @QUERY_PARAMETER@ , @REQUEST_HEADER@ , @REQUEST_BODY@ , @RESPONSE@ , @RESPONSE_HEADER@ , and @RESPONSE_BODY@ . Content inheritance does not apply to any entity of the @API@ , @AUTHORIZER@ , @METHOD@ , @MODEL@ , @REQUEST_BODY@ , or @RESOURCE@ type.
dplType :: Lens' DocumentationPartLocation DocumentationPartType
dplType = lens _dplType (\s a -> s {_dplType = a})

instance FromJSON DocumentationPartLocation where
  parseJSON =
    withObject
      "DocumentationPartLocation"
      ( \x ->
          DocumentationPartLocation'
            <$> (x .:? "path")
            <*> (x .:? "name")
            <*> (x .:? "method")
            <*> (x .:? "statusCode")
            <*> (x .: "type")
      )

instance Hashable DocumentationPartLocation

instance NFData DocumentationPartLocation

instance ToJSON DocumentationPartLocation where
  toJSON DocumentationPartLocation' {..} =
    object
      ( catMaybes
          [ ("path" .=) <$> _dplPath,
            ("name" .=) <$> _dplName,
            ("method" .=) <$> _dplMethod,
            ("statusCode" .=) <$> _dplStatusCode,
            Just ("type" .= _dplType)
          ]
      )
