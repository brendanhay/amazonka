{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchAttachTypedLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchAttachTypedLink where

import Network.AWS.CloudDirectory.Types.AttributeNameAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Attaches a typed link to a specified source and target object inside a
-- BatchRead operation. For more information, see AttachTypedLink and
-- BatchReadRequest$Operations.
--
-- /See:/ 'newBatchAttachTypedLink' smart constructor.
data BatchAttachTypedLink = BatchAttachTypedLink'
  { -- | Identifies the source object that the typed link will attach to.
    sourceObjectReference :: ObjectReference,
    -- | Identifies the target object that the typed link will attach to.
    targetObjectReference :: ObjectReference,
    -- | Identifies the typed link facet that is associated with the typed link.
    typedLinkFacet :: TypedLinkSchemaAndFacetName,
    -- | A set of attributes that are associated with the typed link.
    attributes :: [AttributeNameAndValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchAttachTypedLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceObjectReference', 'batchAttachTypedLink_sourceObjectReference' - Identifies the source object that the typed link will attach to.
--
-- 'targetObjectReference', 'batchAttachTypedLink_targetObjectReference' - Identifies the target object that the typed link will attach to.
--
-- 'typedLinkFacet', 'batchAttachTypedLink_typedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
--
-- 'attributes', 'batchAttachTypedLink_attributes' - A set of attributes that are associated with the typed link.
newBatchAttachTypedLink ::
  -- | 'sourceObjectReference'
  ObjectReference ->
  -- | 'targetObjectReference'
  ObjectReference ->
  -- | 'typedLinkFacet'
  TypedLinkSchemaAndFacetName ->
  BatchAttachTypedLink
newBatchAttachTypedLink
  pSourceObjectReference_
  pTargetObjectReference_
  pTypedLinkFacet_ =
    BatchAttachTypedLink'
      { sourceObjectReference =
          pSourceObjectReference_,
        targetObjectReference = pTargetObjectReference_,
        typedLinkFacet = pTypedLinkFacet_,
        attributes = Prelude.mempty
      }

-- | Identifies the source object that the typed link will attach to.
batchAttachTypedLink_sourceObjectReference :: Lens.Lens' BatchAttachTypedLink ObjectReference
batchAttachTypedLink_sourceObjectReference = Lens.lens (\BatchAttachTypedLink' {sourceObjectReference} -> sourceObjectReference) (\s@BatchAttachTypedLink' {} a -> s {sourceObjectReference = a} :: BatchAttachTypedLink)

-- | Identifies the target object that the typed link will attach to.
batchAttachTypedLink_targetObjectReference :: Lens.Lens' BatchAttachTypedLink ObjectReference
batchAttachTypedLink_targetObjectReference = Lens.lens (\BatchAttachTypedLink' {targetObjectReference} -> targetObjectReference) (\s@BatchAttachTypedLink' {} a -> s {targetObjectReference = a} :: BatchAttachTypedLink)

-- | Identifies the typed link facet that is associated with the typed link.
batchAttachTypedLink_typedLinkFacet :: Lens.Lens' BatchAttachTypedLink TypedLinkSchemaAndFacetName
batchAttachTypedLink_typedLinkFacet = Lens.lens (\BatchAttachTypedLink' {typedLinkFacet} -> typedLinkFacet) (\s@BatchAttachTypedLink' {} a -> s {typedLinkFacet = a} :: BatchAttachTypedLink)

-- | A set of attributes that are associated with the typed link.
batchAttachTypedLink_attributes :: Lens.Lens' BatchAttachTypedLink [AttributeNameAndValue]
batchAttachTypedLink_attributes = Lens.lens (\BatchAttachTypedLink' {attributes} -> attributes) (\s@BatchAttachTypedLink' {} a -> s {attributes = a} :: BatchAttachTypedLink) Prelude.. Prelude._Coerce

instance Prelude.Hashable BatchAttachTypedLink

instance Prelude.NFData BatchAttachTypedLink

instance Prelude.ToJSON BatchAttachTypedLink where
  toJSON BatchAttachTypedLink' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SourceObjectReference"
                  Prelude..= sourceObjectReference
              ),
            Prelude.Just
              ( "TargetObjectReference"
                  Prelude..= targetObjectReference
              ),
            Prelude.Just
              ("TypedLinkFacet" Prelude..= typedLinkFacet),
            Prelude.Just ("Attributes" Prelude..= attributes)
          ]
      )
