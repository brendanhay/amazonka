{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkFacet where

import Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Defines the typed links structure and its attributes. To create a typed link facet, use the 'CreateTypedLinkFacet' API.
--
--
--
-- /See:/ 'typedLinkFacet' smart constructor.
data TypedLinkFacet = TypedLinkFacet'
  { _tlfName :: !Text,
    _tlfAttributes :: ![TypedLinkAttributeDefinition],
    _tlfIdentityAttributeOrder :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TypedLinkFacet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlfName' - The unique name of the typed link facet.
--
-- * 'tlfAttributes' - A set of key-value pairs associated with the typed link. Typed link attributes are used when you have data values that are related to the link itself, and not to one of the two objects being linked. Identity attributes also serve to distinguish the link from others of the same type between the same objects.
--
-- * 'tlfIdentityAttributeOrder' - The set of attributes that distinguish links made from this facet from each other, in the order of significance. Listing typed links can filter on the values of these attributes. See 'ListOutgoingTypedLinks' and 'ListIncomingTypedLinks' for details.
typedLinkFacet ::
  -- | 'tlfName'
  Text ->
  TypedLinkFacet
typedLinkFacet pName_ =
  TypedLinkFacet'
    { _tlfName = pName_,
      _tlfAttributes = mempty,
      _tlfIdentityAttributeOrder = mempty
    }

-- | The unique name of the typed link facet.
tlfName :: Lens' TypedLinkFacet Text
tlfName = lens _tlfName (\s a -> s {_tlfName = a})

-- | A set of key-value pairs associated with the typed link. Typed link attributes are used when you have data values that are related to the link itself, and not to one of the two objects being linked. Identity attributes also serve to distinguish the link from others of the same type between the same objects.
tlfAttributes :: Lens' TypedLinkFacet [TypedLinkAttributeDefinition]
tlfAttributes = lens _tlfAttributes (\s a -> s {_tlfAttributes = a}) . _Coerce

-- | The set of attributes that distinguish links made from this facet from each other, in the order of significance. Listing typed links can filter on the values of these attributes. See 'ListOutgoingTypedLinks' and 'ListIncomingTypedLinks' for details.
tlfIdentityAttributeOrder :: Lens' TypedLinkFacet [Text]
tlfIdentityAttributeOrder = lens _tlfIdentityAttributeOrder (\s a -> s {_tlfIdentityAttributeOrder = a}) . _Coerce

instance Hashable TypedLinkFacet

instance NFData TypedLinkFacet

instance ToJSON TypedLinkFacet where
  toJSON TypedLinkFacet' {..} =
    object
      ( catMaybes
          [ Just ("Name" .= _tlfName),
            Just ("Attributes" .= _tlfAttributes),
            Just ("IdentityAttributeOrder" .= _tlfIdentityAttributeOrder)
          ]
      )
