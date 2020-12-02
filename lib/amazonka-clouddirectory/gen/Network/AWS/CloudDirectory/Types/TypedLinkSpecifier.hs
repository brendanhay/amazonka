{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkSpecifier where

import Network.AWS.CloudDirectory.Types.AttributeNameAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains all the information that is used to uniquely identify a typed link. The parameters discussed in this topic are used to uniquely specify the typed link being operated on. The 'AttachTypedLink' API returns a typed link specifier while the 'DetachTypedLink' API accepts one as input. Similarly, the 'ListIncomingTypedLinks' and 'ListOutgoingTypedLinks' API operations provide typed link specifiers as output. You can also construct a typed link specifier from scratch.
--
--
--
-- /See:/ 'typedLinkSpecifier' smart constructor.
data TypedLinkSpecifier = TypedLinkSpecifier'
  { _tlsTypedLinkFacet ::
      !TypedLinkSchemaAndFacetName,
    _tlsSourceObjectReference :: !ObjectReference,
    _tlsTargetObjectReference :: !ObjectReference,
    _tlsIdentityAttributeValues ::
      ![AttributeNameAndValue]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TypedLinkSpecifier' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlsTypedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
--
-- * 'tlsSourceObjectReference' - Identifies the source object that the typed link will attach to.
--
-- * 'tlsTargetObjectReference' - Identifies the target object that the typed link will attach to.
--
-- * 'tlsIdentityAttributeValues' - Identifies the attribute value to update.
typedLinkSpecifier ::
  -- | 'tlsTypedLinkFacet'
  TypedLinkSchemaAndFacetName ->
  -- | 'tlsSourceObjectReference'
  ObjectReference ->
  -- | 'tlsTargetObjectReference'
  ObjectReference ->
  TypedLinkSpecifier
typedLinkSpecifier
  pTypedLinkFacet_
  pSourceObjectReference_
  pTargetObjectReference_ =
    TypedLinkSpecifier'
      { _tlsTypedLinkFacet = pTypedLinkFacet_,
        _tlsSourceObjectReference = pSourceObjectReference_,
        _tlsTargetObjectReference = pTargetObjectReference_,
        _tlsIdentityAttributeValues = mempty
      }

-- | Identifies the typed link facet that is associated with the typed link.
tlsTypedLinkFacet :: Lens' TypedLinkSpecifier TypedLinkSchemaAndFacetName
tlsTypedLinkFacet = lens _tlsTypedLinkFacet (\s a -> s {_tlsTypedLinkFacet = a})

-- | Identifies the source object that the typed link will attach to.
tlsSourceObjectReference :: Lens' TypedLinkSpecifier ObjectReference
tlsSourceObjectReference = lens _tlsSourceObjectReference (\s a -> s {_tlsSourceObjectReference = a})

-- | Identifies the target object that the typed link will attach to.
tlsTargetObjectReference :: Lens' TypedLinkSpecifier ObjectReference
tlsTargetObjectReference = lens _tlsTargetObjectReference (\s a -> s {_tlsTargetObjectReference = a})

-- | Identifies the attribute value to update.
tlsIdentityAttributeValues :: Lens' TypedLinkSpecifier [AttributeNameAndValue]
tlsIdentityAttributeValues = lens _tlsIdentityAttributeValues (\s a -> s {_tlsIdentityAttributeValues = a}) . _Coerce

instance FromJSON TypedLinkSpecifier where
  parseJSON =
    withObject
      "TypedLinkSpecifier"
      ( \x ->
          TypedLinkSpecifier'
            <$> (x .: "TypedLinkFacet")
            <*> (x .: "SourceObjectReference")
            <*> (x .: "TargetObjectReference")
            <*> (x .:? "IdentityAttributeValues" .!= mempty)
      )

instance Hashable TypedLinkSpecifier

instance NFData TypedLinkSpecifier

instance ToJSON TypedLinkSpecifier where
  toJSON TypedLinkSpecifier' {..} =
    object
      ( catMaybes
          [ Just ("TypedLinkFacet" .= _tlsTypedLinkFacet),
            Just ("SourceObjectReference" .= _tlsSourceObjectReference),
            Just ("TargetObjectReference" .= _tlsTargetObjectReference),
            Just ("IdentityAttributeValues" .= _tlsIdentityAttributeValues)
          ]
      )
