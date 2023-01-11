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
-- Module      : Amazonka.CloudDirectory.Types.AttributeKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.AttributeKey where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON AttributeKey where
  parseJSON =
    Data.withObject
      "AttributeKey"
      ( \x ->
          AttributeKey'
            Prelude.<$> (x Data..: "SchemaArn")
            Prelude.<*> (x Data..: "FacetName")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable AttributeKey where
  hashWithSalt _salt AttributeKey' {..} =
    _salt `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` facetName
      `Prelude.hashWithSalt` name

instance Prelude.NFData AttributeKey where
  rnf AttributeKey' {..} =
    Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf facetName
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON AttributeKey where
  toJSON AttributeKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaArn" Data..= schemaArn),
            Prelude.Just ("FacetName" Data..= facetName),
            Prelude.Just ("Name" Data..= name)
          ]
      )
