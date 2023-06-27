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
-- Module      : Amazonka.CloudDirectory.Types.TypedLinkFacet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.TypedLinkFacet where

import Amazonka.CloudDirectory.Types.TypedLinkAttributeDefinition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the typed links structure and its attributes. To create a typed
-- link facet, use the CreateTypedLinkFacet API.
--
-- /See:/ 'newTypedLinkFacet' smart constructor.
data TypedLinkFacet = TypedLinkFacet'
  { -- | The unique name of the typed link facet.
    name :: Prelude.Text,
    -- | A set of key-value pairs associated with the typed link. Typed link
    -- attributes are used when you have data values that are related to the
    -- link itself, and not to one of the two objects being linked. Identity
    -- attributes also serve to distinguish the link from others of the same
    -- type between the same objects.
    attributes :: [TypedLinkAttributeDefinition],
    -- | The set of attributes that distinguish links made from this facet from
    -- each other, in the order of significance. Listing typed links can filter
    -- on the values of these attributes. See ListOutgoingTypedLinks and
    -- ListIncomingTypedLinks for details.
    identityAttributeOrder :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TypedLinkFacet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'typedLinkFacet_name' - The unique name of the typed link facet.
--
-- 'attributes', 'typedLinkFacet_attributes' - A set of key-value pairs associated with the typed link. Typed link
-- attributes are used when you have data values that are related to the
-- link itself, and not to one of the two objects being linked. Identity
-- attributes also serve to distinguish the link from others of the same
-- type between the same objects.
--
-- 'identityAttributeOrder', 'typedLinkFacet_identityAttributeOrder' - The set of attributes that distinguish links made from this facet from
-- each other, in the order of significance. Listing typed links can filter
-- on the values of these attributes. See ListOutgoingTypedLinks and
-- ListIncomingTypedLinks for details.
newTypedLinkFacet ::
  -- | 'name'
  Prelude.Text ->
  TypedLinkFacet
newTypedLinkFacet pName_ =
  TypedLinkFacet'
    { name = pName_,
      attributes = Prelude.mempty,
      identityAttributeOrder = Prelude.mempty
    }

-- | The unique name of the typed link facet.
typedLinkFacet_name :: Lens.Lens' TypedLinkFacet Prelude.Text
typedLinkFacet_name = Lens.lens (\TypedLinkFacet' {name} -> name) (\s@TypedLinkFacet' {} a -> s {name = a} :: TypedLinkFacet)

-- | A set of key-value pairs associated with the typed link. Typed link
-- attributes are used when you have data values that are related to the
-- link itself, and not to one of the two objects being linked. Identity
-- attributes also serve to distinguish the link from others of the same
-- type between the same objects.
typedLinkFacet_attributes :: Lens.Lens' TypedLinkFacet [TypedLinkAttributeDefinition]
typedLinkFacet_attributes = Lens.lens (\TypedLinkFacet' {attributes} -> attributes) (\s@TypedLinkFacet' {} a -> s {attributes = a} :: TypedLinkFacet) Prelude.. Lens.coerced

-- | The set of attributes that distinguish links made from this facet from
-- each other, in the order of significance. Listing typed links can filter
-- on the values of these attributes. See ListOutgoingTypedLinks and
-- ListIncomingTypedLinks for details.
typedLinkFacet_identityAttributeOrder :: Lens.Lens' TypedLinkFacet [Prelude.Text]
typedLinkFacet_identityAttributeOrder = Lens.lens (\TypedLinkFacet' {identityAttributeOrder} -> identityAttributeOrder) (\s@TypedLinkFacet' {} a -> s {identityAttributeOrder = a} :: TypedLinkFacet) Prelude.. Lens.coerced

instance Prelude.Hashable TypedLinkFacet where
  hashWithSalt _salt TypedLinkFacet' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` identityAttributeOrder

instance Prelude.NFData TypedLinkFacet where
  rnf TypedLinkFacet' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf identityAttributeOrder

instance Data.ToJSON TypedLinkFacet where
  toJSON TypedLinkFacet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Attributes" Data..= attributes),
            Prelude.Just
              ( "IdentityAttributeOrder"
                  Data..= identityAttributeOrder
              )
          ]
      )
