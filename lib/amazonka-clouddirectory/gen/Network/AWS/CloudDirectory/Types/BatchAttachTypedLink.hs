{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachTypedLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachTypedLink where

import Network.AWS.CloudDirectory.Types.AttributeNameAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Attaches a typed link to a specified source and target object inside a 'BatchRead' operation. For more information, see 'AttachTypedLink' and 'BatchReadRequest$Operations' .
--
--
--
-- /See:/ 'batchAttachTypedLink' smart constructor.
data BatchAttachTypedLink = BatchAttachTypedLink'
  { _batlSourceObjectReference ::
      !ObjectReference,
    _batlTargetObjectReference :: !ObjectReference,
    _batlTypedLinkFacet ::
      !TypedLinkSchemaAndFacetName,
    _batlAttributes :: ![AttributeNameAndValue]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchAttachTypedLink' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'batlSourceObjectReference' - Identifies the source object that the typed link will attach to.
--
-- * 'batlTargetObjectReference' - Identifies the target object that the typed link will attach to.
--
-- * 'batlTypedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
--
-- * 'batlAttributes' - A set of attributes that are associated with the typed link.
batchAttachTypedLink ::
  -- | 'batlSourceObjectReference'
  ObjectReference ->
  -- | 'batlTargetObjectReference'
  ObjectReference ->
  -- | 'batlTypedLinkFacet'
  TypedLinkSchemaAndFacetName ->
  BatchAttachTypedLink
batchAttachTypedLink
  pSourceObjectReference_
  pTargetObjectReference_
  pTypedLinkFacet_ =
    BatchAttachTypedLink'
      { _batlSourceObjectReference =
          pSourceObjectReference_,
        _batlTargetObjectReference = pTargetObjectReference_,
        _batlTypedLinkFacet = pTypedLinkFacet_,
        _batlAttributes = mempty
      }

-- | Identifies the source object that the typed link will attach to.
batlSourceObjectReference :: Lens' BatchAttachTypedLink ObjectReference
batlSourceObjectReference = lens _batlSourceObjectReference (\s a -> s {_batlSourceObjectReference = a})

-- | Identifies the target object that the typed link will attach to.
batlTargetObjectReference :: Lens' BatchAttachTypedLink ObjectReference
batlTargetObjectReference = lens _batlTargetObjectReference (\s a -> s {_batlTargetObjectReference = a})

-- | Identifies the typed link facet that is associated with the typed link.
batlTypedLinkFacet :: Lens' BatchAttachTypedLink TypedLinkSchemaAndFacetName
batlTypedLinkFacet = lens _batlTypedLinkFacet (\s a -> s {_batlTypedLinkFacet = a})

-- | A set of attributes that are associated with the typed link.
batlAttributes :: Lens' BatchAttachTypedLink [AttributeNameAndValue]
batlAttributes = lens _batlAttributes (\s a -> s {_batlAttributes = a}) . _Coerce

instance Hashable BatchAttachTypedLink

instance NFData BatchAttachTypedLink

instance ToJSON BatchAttachTypedLink where
  toJSON BatchAttachTypedLink' {..} =
    object
      ( catMaybes
          [ Just ("SourceObjectReference" .= _batlSourceObjectReference),
            Just ("TargetObjectReference" .= _batlTargetObjectReference),
            Just ("TypedLinkFacet" .= _batlTypedLinkFacet),
            Just ("Attributes" .= _batlAttributes)
          ]
      )
