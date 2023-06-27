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
-- Module      : Amazonka.CloudDirectory.Types.Facet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.Facet where

import Amazonka.CloudDirectory.Types.FacetStyle
import Amazonka.CloudDirectory.Types.ObjectType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains @Name@, @ARN@, @Attributes@, @ @@Rule@@s@, and
-- @ObjectTypes@. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_whatarefacets.html Facets>
-- for more information.
--
-- /See:/ 'newFacet' smart constructor.
data Facet = Facet'
  { -- | There are two different styles that you can define on any given facet,
    -- @Static@ and @Dynamic@. For static facets, all attributes must be
    -- defined in the schema. For dynamic facets, attributes can be defined
    -- during data plane operations.
    facetStyle :: Prelude.Maybe FacetStyle,
    -- | The name of the Facet.
    name :: Prelude.Maybe Prelude.Text,
    -- | The object type that is associated with the facet. See
    -- CreateFacetRequest$ObjectType for more details.
    objectType :: Prelude.Maybe ObjectType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Facet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'facetStyle', 'facet_facetStyle' - There are two different styles that you can define on any given facet,
-- @Static@ and @Dynamic@. For static facets, all attributes must be
-- defined in the schema. For dynamic facets, attributes can be defined
-- during data plane operations.
--
-- 'name', 'facet_name' - The name of the Facet.
--
-- 'objectType', 'facet_objectType' - The object type that is associated with the facet. See
-- CreateFacetRequest$ObjectType for more details.
newFacet ::
  Facet
newFacet =
  Facet'
    { facetStyle = Prelude.Nothing,
      name = Prelude.Nothing,
      objectType = Prelude.Nothing
    }

-- | There are two different styles that you can define on any given facet,
-- @Static@ and @Dynamic@. For static facets, all attributes must be
-- defined in the schema. For dynamic facets, attributes can be defined
-- during data plane operations.
facet_facetStyle :: Lens.Lens' Facet (Prelude.Maybe FacetStyle)
facet_facetStyle = Lens.lens (\Facet' {facetStyle} -> facetStyle) (\s@Facet' {} a -> s {facetStyle = a} :: Facet)

-- | The name of the Facet.
facet_name :: Lens.Lens' Facet (Prelude.Maybe Prelude.Text)
facet_name = Lens.lens (\Facet' {name} -> name) (\s@Facet' {} a -> s {name = a} :: Facet)

-- | The object type that is associated with the facet. See
-- CreateFacetRequest$ObjectType for more details.
facet_objectType :: Lens.Lens' Facet (Prelude.Maybe ObjectType)
facet_objectType = Lens.lens (\Facet' {objectType} -> objectType) (\s@Facet' {} a -> s {objectType = a} :: Facet)

instance Data.FromJSON Facet where
  parseJSON =
    Data.withObject
      "Facet"
      ( \x ->
          Facet'
            Prelude.<$> (x Data..:? "FacetStyle")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ObjectType")
      )

instance Prelude.Hashable Facet where
  hashWithSalt _salt Facet' {..} =
    _salt
      `Prelude.hashWithSalt` facetStyle
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` objectType

instance Prelude.NFData Facet where
  rnf Facet' {..} =
    Prelude.rnf facetStyle
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf objectType
