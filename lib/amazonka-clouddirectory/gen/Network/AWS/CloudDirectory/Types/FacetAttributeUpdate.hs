{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeUpdate where

import Network.AWS.CloudDirectory.Types.FacetAttribute
import Network.AWS.CloudDirectory.Types.UpdateActionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A structure that contains information used to update an attribute.
--
--
--
-- /See:/ 'facetAttributeUpdate' smart constructor.
data FacetAttributeUpdate = FacetAttributeUpdate'
  { _fauAttribute ::
      !(Maybe FacetAttribute),
    _fauAction :: !(Maybe UpdateActionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FacetAttributeUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fauAttribute' - The attribute to update.
--
-- * 'fauAction' - The action to perform when updating the attribute.
facetAttributeUpdate ::
  FacetAttributeUpdate
facetAttributeUpdate =
  FacetAttributeUpdate'
    { _fauAttribute = Nothing,
      _fauAction = Nothing
    }

-- | The attribute to update.
fauAttribute :: Lens' FacetAttributeUpdate (Maybe FacetAttribute)
fauAttribute = lens _fauAttribute (\s a -> s {_fauAttribute = a})

-- | The action to perform when updating the attribute.
fauAction :: Lens' FacetAttributeUpdate (Maybe UpdateActionType)
fauAction = lens _fauAction (\s a -> s {_fauAction = a})

instance Hashable FacetAttributeUpdate

instance NFData FacetAttributeUpdate

instance ToJSON FacetAttributeUpdate where
  toJSON FacetAttributeUpdate' {..} =
    object
      ( catMaybes
          [("Attribute" .=) <$> _fauAttribute, ("Action" .=) <$> _fauAction]
      )
