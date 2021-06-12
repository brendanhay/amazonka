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
-- Module      : Network.AWS.CloudDirectory.Types.Facet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.Facet where

import Network.AWS.CloudDirectory.Types.FacetStyle
import Network.AWS.CloudDirectory.Types.ObjectType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure that contains @Name@, @ARN@, @Attributes@, @ Rules@, and
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
    facetStyle :: Core.Maybe FacetStyle,
    -- | The name of the Facet.
    name :: Core.Maybe Core.Text,
    -- | The object type that is associated with the facet. See
    -- CreateFacetRequest$ObjectType for more details.
    objectType :: Core.Maybe ObjectType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { facetStyle = Core.Nothing,
      name = Core.Nothing,
      objectType = Core.Nothing
    }

-- | There are two different styles that you can define on any given facet,
-- @Static@ and @Dynamic@. For static facets, all attributes must be
-- defined in the schema. For dynamic facets, attributes can be defined
-- during data plane operations.
facet_facetStyle :: Lens.Lens' Facet (Core.Maybe FacetStyle)
facet_facetStyle = Lens.lens (\Facet' {facetStyle} -> facetStyle) (\s@Facet' {} a -> s {facetStyle = a} :: Facet)

-- | The name of the Facet.
facet_name :: Lens.Lens' Facet (Core.Maybe Core.Text)
facet_name = Lens.lens (\Facet' {name} -> name) (\s@Facet' {} a -> s {name = a} :: Facet)

-- | The object type that is associated with the facet. See
-- CreateFacetRequest$ObjectType for more details.
facet_objectType :: Lens.Lens' Facet (Core.Maybe ObjectType)
facet_objectType = Lens.lens (\Facet' {objectType} -> objectType) (\s@Facet' {} a -> s {objectType = a} :: Facet)

instance Core.FromJSON Facet where
  parseJSON =
    Core.withObject
      "Facet"
      ( \x ->
          Facet'
            Core.<$> (x Core..:? "FacetStyle")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "ObjectType")
      )

instance Core.Hashable Facet

instance Core.NFData Facet
