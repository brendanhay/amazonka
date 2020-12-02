{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Facet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.Facet where

import Network.AWS.CloudDirectory.Types.FacetStyle
import Network.AWS.CloudDirectory.Types.ObjectType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that contains @Name@ , @ARN@ , @Attributes@ , @'Rule' s@ , and @ObjectTypes@ . See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_whatarefacets.html Facets> for more information.
--
--
--
-- /See:/ 'facet' smart constructor.
data Facet = Facet'
  { _fFacetStyle :: !(Maybe FacetStyle),
    _fObjectType :: !(Maybe ObjectType),
    _fName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Facet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fFacetStyle' - There are two different styles that you can define on any given facet, @Static@ and @Dynamic@ . For static facets, all attributes must be defined in the schema. For dynamic facets, attributes can be defined during data plane operations.
--
-- * 'fObjectType' - The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
--
-- * 'fName' - The name of the 'Facet' .
facet ::
  Facet
facet =
  Facet'
    { _fFacetStyle = Nothing,
      _fObjectType = Nothing,
      _fName = Nothing
    }

-- | There are two different styles that you can define on any given facet, @Static@ and @Dynamic@ . For static facets, all attributes must be defined in the schema. For dynamic facets, attributes can be defined during data plane operations.
fFacetStyle :: Lens' Facet (Maybe FacetStyle)
fFacetStyle = lens _fFacetStyle (\s a -> s {_fFacetStyle = a})

-- | The object type that is associated with the facet. See 'CreateFacetRequest$ObjectType' for more details.
fObjectType :: Lens' Facet (Maybe ObjectType)
fObjectType = lens _fObjectType (\s a -> s {_fObjectType = a})

-- | The name of the 'Facet' .
fName :: Lens' Facet (Maybe Text)
fName = lens _fName (\s a -> s {_fName = a})

instance FromJSON Facet where
  parseJSON =
    withObject
      "Facet"
      ( \x ->
          Facet'
            <$> (x .:? "FacetStyle") <*> (x .:? "ObjectType") <*> (x .:? "Name")
      )

instance Hashable Facet

instance NFData Facet
