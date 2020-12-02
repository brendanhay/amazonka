{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkFacetAttributeUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkFacetAttributeUpdate where

import Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
import Network.AWS.CloudDirectory.Types.UpdateActionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A typed link facet attribute update.
--
--
--
-- /See:/ 'typedLinkFacetAttributeUpdate' smart constructor.
data TypedLinkFacetAttributeUpdate = TypedLinkFacetAttributeUpdate'
  { _tlfauAttribute ::
      !TypedLinkAttributeDefinition,
    _tlfauAction ::
      !UpdateActionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TypedLinkFacetAttributeUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlfauAttribute' - The attribute to update.
--
-- * 'tlfauAction' - The action to perform when updating the attribute.
typedLinkFacetAttributeUpdate ::
  -- | 'tlfauAttribute'
  TypedLinkAttributeDefinition ->
  -- | 'tlfauAction'
  UpdateActionType ->
  TypedLinkFacetAttributeUpdate
typedLinkFacetAttributeUpdate pAttribute_ pAction_ =
  TypedLinkFacetAttributeUpdate'
    { _tlfauAttribute = pAttribute_,
      _tlfauAction = pAction_
    }

-- | The attribute to update.
tlfauAttribute :: Lens' TypedLinkFacetAttributeUpdate TypedLinkAttributeDefinition
tlfauAttribute = lens _tlfauAttribute (\s a -> s {_tlfauAttribute = a})

-- | The action to perform when updating the attribute.
tlfauAction :: Lens' TypedLinkFacetAttributeUpdate UpdateActionType
tlfauAction = lens _tlfauAction (\s a -> s {_tlfauAction = a})

instance Hashable TypedLinkFacetAttributeUpdate

instance NFData TypedLinkFacetAttributeUpdate

instance ToJSON TypedLinkFacetAttributeUpdate where
  toJSON TypedLinkFacetAttributeUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Attribute" .= _tlfauAttribute),
            Just ("Action" .= _tlfauAction)
          ]
      )
