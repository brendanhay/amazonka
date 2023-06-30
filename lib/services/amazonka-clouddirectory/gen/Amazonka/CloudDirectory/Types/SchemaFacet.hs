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
-- Module      : Amazonka.CloudDirectory.Types.SchemaFacet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.SchemaFacet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A facet.
--
-- /See:/ 'newSchemaFacet' smart constructor.
data SchemaFacet = SchemaFacet'
  { -- | The name of the facet. If this value is set, SchemaArn must also be set.
    facetName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the schema that contains the facet with no minor component.
    -- See arns and
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_inplaceschemaupgrade.html In-Place Schema Upgrade>
    -- for a description of when to provide minor versions. If this value is
    -- set, FacetName must also be set.
    schemaArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaFacet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'facetName', 'schemaFacet_facetName' - The name of the facet. If this value is set, SchemaArn must also be set.
--
-- 'schemaArn', 'schemaFacet_schemaArn' - The ARN of the schema that contains the facet with no minor component.
-- See arns and
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_inplaceschemaupgrade.html In-Place Schema Upgrade>
-- for a description of when to provide minor versions. If this value is
-- set, FacetName must also be set.
newSchemaFacet ::
  SchemaFacet
newSchemaFacet =
  SchemaFacet'
    { facetName = Prelude.Nothing,
      schemaArn = Prelude.Nothing
    }

-- | The name of the facet. If this value is set, SchemaArn must also be set.
schemaFacet_facetName :: Lens.Lens' SchemaFacet (Prelude.Maybe Prelude.Text)
schemaFacet_facetName = Lens.lens (\SchemaFacet' {facetName} -> facetName) (\s@SchemaFacet' {} a -> s {facetName = a} :: SchemaFacet)

-- | The ARN of the schema that contains the facet with no minor component.
-- See arns and
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_inplaceschemaupgrade.html In-Place Schema Upgrade>
-- for a description of when to provide minor versions. If this value is
-- set, FacetName must also be set.
schemaFacet_schemaArn :: Lens.Lens' SchemaFacet (Prelude.Maybe Prelude.Text)
schemaFacet_schemaArn = Lens.lens (\SchemaFacet' {schemaArn} -> schemaArn) (\s@SchemaFacet' {} a -> s {schemaArn = a} :: SchemaFacet)

instance Data.FromJSON SchemaFacet where
  parseJSON =
    Data.withObject
      "SchemaFacet"
      ( \x ->
          SchemaFacet'
            Prelude.<$> (x Data..:? "FacetName")
            Prelude.<*> (x Data..:? "SchemaArn")
      )

instance Prelude.Hashable SchemaFacet where
  hashWithSalt _salt SchemaFacet' {..} =
    _salt
      `Prelude.hashWithSalt` facetName
      `Prelude.hashWithSalt` schemaArn

instance Prelude.NFData SchemaFacet where
  rnf SchemaFacet' {..} =
    Prelude.rnf facetName
      `Prelude.seq` Prelude.rnf schemaArn

instance Data.ToJSON SchemaFacet where
  toJSON SchemaFacet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FacetName" Data..=) Prelude.<$> facetName,
            ("SchemaArn" Data..=) Prelude.<$> schemaArn
          ]
      )
