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
-- Module      : Amazonka.CloudDirectory.Types.TypedLinkSpecifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.TypedLinkSpecifier where

import Amazonka.CloudDirectory.Types.AttributeNameAndValue
import Amazonka.CloudDirectory.Types.ObjectReference
import Amazonka.CloudDirectory.Types.TypedLinkSchemaAndFacetName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
typedLinkSpecifier_identityAttributeValues = Lens.lens (\TypedLinkSpecifier' {identityAttributeValues} -> identityAttributeValues) (\s@TypedLinkSpecifier' {} a -> s {identityAttributeValues = a} :: TypedLinkSpecifier) Prelude.. Lens.coerced

instance Data.FromJSON TypedLinkSpecifier where
  parseJSON =
    Data.withObject
      "TypedLinkSpecifier"
      ( \x ->
          TypedLinkSpecifier'
            Prelude.<$> (x Data..: "TypedLinkFacet")
            Prelude.<*> (x Data..: "SourceObjectReference")
            Prelude.<*> (x Data..: "TargetObjectReference")
            Prelude.<*> ( x
                            Data..:? "IdentityAttributeValues"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable TypedLinkSpecifier where
  hashWithSalt _salt TypedLinkSpecifier' {..} =
    _salt
      `Prelude.hashWithSalt` typedLinkFacet
      `Prelude.hashWithSalt` sourceObjectReference
      `Prelude.hashWithSalt` targetObjectReference
      `Prelude.hashWithSalt` identityAttributeValues

instance Prelude.NFData TypedLinkSpecifier where
  rnf TypedLinkSpecifier' {..} =
    Prelude.rnf typedLinkFacet
      `Prelude.seq` Prelude.rnf sourceObjectReference
      `Prelude.seq` Prelude.rnf targetObjectReference
      `Prelude.seq` Prelude.rnf identityAttributeValues

instance Data.ToJSON TypedLinkSpecifier where
  toJSON TypedLinkSpecifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TypedLinkFacet" Data..= typedLinkFacet),
            Prelude.Just
              ( "SourceObjectReference"
                  Data..= sourceObjectReference
              ),
            Prelude.Just
              ( "TargetObjectReference"
                  Data..= targetObjectReference
              ),
            Prelude.Just
              ( "IdentityAttributeValues"
                  Data..= identityAttributeValues
              )
          ]
      )
