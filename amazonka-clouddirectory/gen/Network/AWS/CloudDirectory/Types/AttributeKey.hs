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
-- Module      : Network.AWS.CloudDirectory.Types.AttributeKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.AttributeKey where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A unique identifier for an attribute.
--
-- /See:/ 'newAttributeKey' smart constructor.
data AttributeKey = AttributeKey'
  { -- | The Amazon Resource Name (ARN) of the schema that contains the facet and
    -- attribute.
    schemaArn :: Prelude.Text,
    -- | The name of the facet that the attribute exists within.
    facetName :: Prelude.Text,
    -- | The name of the attribute.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttributeKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'attributeKey_schemaArn' - The Amazon Resource Name (ARN) of the schema that contains the facet and
-- attribute.
--
-- 'facetName', 'attributeKey_facetName' - The name of the facet that the attribute exists within.
--
-- 'name', 'attributeKey_name' - The name of the attribute.
newAttributeKey ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'facetName'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  AttributeKey
newAttributeKey pSchemaArn_ pFacetName_ pName_ =
  AttributeKey'
    { schemaArn = pSchemaArn_,
      facetName = pFacetName_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) of the schema that contains the facet and
-- attribute.
attributeKey_schemaArn :: Lens.Lens' AttributeKey Prelude.Text
attributeKey_schemaArn = Lens.lens (\AttributeKey' {schemaArn} -> schemaArn) (\s@AttributeKey' {} a -> s {schemaArn = a} :: AttributeKey)

-- | The name of the facet that the attribute exists within.
attributeKey_facetName :: Lens.Lens' AttributeKey Prelude.Text
attributeKey_facetName = Lens.lens (\AttributeKey' {facetName} -> facetName) (\s@AttributeKey' {} a -> s {facetName = a} :: AttributeKey)

-- | The name of the attribute.
attributeKey_name :: Lens.Lens' AttributeKey Prelude.Text
attributeKey_name = Lens.lens (\AttributeKey' {name} -> name) (\s@AttributeKey' {} a -> s {name = a} :: AttributeKey)

instance Prelude.FromJSON AttributeKey where
  parseJSON =
    Prelude.withObject
      "AttributeKey"
      ( \x ->
          AttributeKey'
            Prelude.<$> (x Prelude..: "SchemaArn")
            Prelude.<*> (x Prelude..: "FacetName")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable AttributeKey

instance Prelude.NFData AttributeKey

instance Prelude.ToJSON AttributeKey where
  toJSON AttributeKey' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaArn" Prelude..= schemaArn),
            Prelude.Just ("FacetName" Prelude..= facetName),
            Prelude.Just ("Name" Prelude..= name)
          ]
      )
