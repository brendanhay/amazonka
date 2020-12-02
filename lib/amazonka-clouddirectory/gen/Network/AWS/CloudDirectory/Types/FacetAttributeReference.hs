{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeReference where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The facet attribute reference that specifies the attribute definition that contains the attribute facet name and attribute name.
--
--
--
-- /See:/ 'facetAttributeReference' smart constructor.
data FacetAttributeReference = FacetAttributeReference'
  { _farTargetFacetName ::
      !Text,
    _farTargetAttributeName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FacetAttributeReference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'farTargetFacetName' - The target facet name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- * 'farTargetAttributeName' - The target attribute name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
facetAttributeReference ::
  -- | 'farTargetFacetName'
  Text ->
  -- | 'farTargetAttributeName'
  Text ->
  FacetAttributeReference
facetAttributeReference pTargetFacetName_ pTargetAttributeName_ =
  FacetAttributeReference'
    { _farTargetFacetName = pTargetFacetName_,
      _farTargetAttributeName = pTargetAttributeName_
    }

-- | The target facet name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
farTargetFacetName :: Lens' FacetAttributeReference Text
farTargetFacetName = lens _farTargetFacetName (\s a -> s {_farTargetFacetName = a})

-- | The target attribute name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
farTargetAttributeName :: Lens' FacetAttributeReference Text
farTargetAttributeName = lens _farTargetAttributeName (\s a -> s {_farTargetAttributeName = a})

instance FromJSON FacetAttributeReference where
  parseJSON =
    withObject
      "FacetAttributeReference"
      ( \x ->
          FacetAttributeReference'
            <$> (x .: "TargetFacetName") <*> (x .: "TargetAttributeName")
      )

instance Hashable FacetAttributeReference

instance NFData FacetAttributeReference

instance ToJSON FacetAttributeReference where
  toJSON FacetAttributeReference' {..} =
    object
      ( catMaybes
          [ Just ("TargetFacetName" .= _farTargetFacetName),
            Just ("TargetAttributeName" .= _farTargetAttributeName)
          ]
      )
