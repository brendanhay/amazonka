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
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkSpecifier where

import Network.AWS.CloudDirectory.Types.AttributeNameAndValue
import Network.AWS.CloudDirectory.Types.ObjectReference
import Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains all the information that is used to uniquely identify a typed
-- link. The parameters discussed in this topic are used to uniquely
-- specify the typed link being operated on. The AttachTypedLink API
-- returns a typed link specifier while the DetachTypedLink API accepts one
-- as input. Similarly, the ListIncomingTypedLinks and
-- ListOutgoingTypedLinks API operations provide typed link specifiers as
-- output. You can also construct a typed link specifier from scratch.
--
-- /See:/ 'newTypedLinkSpecifier' smart constructor.
data TypedLinkSpecifier = TypedLinkSpecifier'
  { -- | Identifies the typed link facet that is associated with the typed link.
    typedLinkFacet :: TypedLinkSchemaAndFacetName,
    -- | Identifies the source object that the typed link will attach to.
    sourceObjectReference :: ObjectReference,
    -- | Identifies the target object that the typed link will attach to.
    targetObjectReference :: ObjectReference,
    -- | Identifies the attribute value to update.
    identityAttributeValues :: [AttributeNameAndValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TypedLinkSpecifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typedLinkFacet', 'typedLinkSpecifier_typedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
--
-- 'sourceObjectReference', 'typedLinkSpecifier_sourceObjectReference' - Identifies the source object that the typed link will attach to.
--
-- 'targetObjectReference', 'typedLinkSpecifier_targetObjectReference' - Identifies the target object that the typed link will attach to.
--
-- 'identityAttributeValues', 'typedLinkSpecifier_identityAttributeValues' - Identifies the attribute value to update.
newTypedLinkSpecifier ::
  -- | 'typedLinkFacet'
  TypedLinkSchemaAndFacetName ->
  -- | 'sourceObjectReference'
  ObjectReference ->
  -- | 'targetObjectReference'
  ObjectReference ->
  TypedLinkSpecifier
newTypedLinkSpecifier
  pTypedLinkFacet_
  pSourceObjectReference_
  pTargetObjectReference_ =
    TypedLinkSpecifier'
      { typedLinkFacet =
          pTypedLinkFacet_,
        sourceObjectReference = pSourceObjectReference_,
        targetObjectReference = pTargetObjectReference_,
        identityAttributeValues = Prelude.mempty
      }

-- | Identifies the typed link facet that is associated with the typed link.
typedLinkSpecifier_typedLinkFacet :: Lens.Lens' TypedLinkSpecifier TypedLinkSchemaAndFacetName
typedLinkSpecifier_typedLinkFacet = Lens.lens (\TypedLinkSpecifier' {typedLinkFacet} -> typedLinkFacet) (\s@TypedLinkSpecifier' {} a -> s {typedLinkFacet = a} :: TypedLinkSpecifier)

-- | Identifies the source object that the typed link will attach to.
typedLinkSpecifier_sourceObjectReference :: Lens.Lens' TypedLinkSpecifier ObjectReference
typedLinkSpecifier_sourceObjectReference = Lens.lens (\TypedLinkSpecifier' {sourceObjectReference} -> sourceObjectReference) (\s@TypedLinkSpecifier' {} a -> s {sourceObjectReference = a} :: TypedLinkSpecifier)

-- | Identifies the target object that the typed link will attach to.
typedLinkSpecifier_targetObjectReference :: Lens.Lens' TypedLinkSpecifier ObjectReference
typedLinkSpecifier_targetObjectReference = Lens.lens (\TypedLinkSpecifier' {targetObjectReference} -> targetObjectReference) (\s@TypedLinkSpecifier' {} a -> s {targetObjectReference = a} :: TypedLinkSpecifier)

-- | Identifies the attribute value to update.
typedLinkSpecifier_identityAttributeValues :: Lens.Lens' TypedLinkSpecifier [AttributeNameAndValue]
typedLinkSpecifier_identityAttributeValues = Lens.lens (\TypedLinkSpecifier' {identityAttributeValues} -> identityAttributeValues) (\s@TypedLinkSpecifier' {} a -> s {identityAttributeValues = a} :: TypedLinkSpecifier) Prelude.. Prelude._Coerce

instance Prelude.FromJSON TypedLinkSpecifier where
  parseJSON =
    Prelude.withObject
      "TypedLinkSpecifier"
      ( \x ->
          TypedLinkSpecifier'
            Prelude.<$> (x Prelude..: "TypedLinkFacet")
            Prelude.<*> (x Prelude..: "SourceObjectReference")
            Prelude.<*> (x Prelude..: "TargetObjectReference")
            Prelude.<*> ( x Prelude..:? "IdentityAttributeValues"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TypedLinkSpecifier

instance Prelude.NFData TypedLinkSpecifier

instance Prelude.ToJSON TypedLinkSpecifier where
  toJSON TypedLinkSpecifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TypedLinkFacet" Prelude..= typedLinkFacet),
            Prelude.Just
              ( "SourceObjectReference"
                  Prelude..= sourceObjectReference
              ),
            Prelude.Just
              ( "TargetObjectReference"
                  Prelude..= targetObjectReference
              ),
            Prelude.Just
              ( "IdentityAttributeValues"
                  Prelude..= identityAttributeValues
              )
          ]
      )
