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
-- Module      : Amazonka.CloudDirectory.Types.TypedLinkSchemaAndFacetName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.TypedLinkSchemaAndFacetName where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifies the schema Amazon Resource Name (ARN) and facet name for the
-- typed link.
--
-- /See:/ 'newTypedLinkSchemaAndFacetName' smart constructor.
data TypedLinkSchemaAndFacetName = TypedLinkSchemaAndFacetName'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Prelude.Text,
    -- | The unique name of the typed link facet.
    typedLinkName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TypedLinkSchemaAndFacetName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'typedLinkSchemaAndFacetName_schemaArn' - The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
--
-- 'typedLinkName', 'typedLinkSchemaAndFacetName_typedLinkName' - The unique name of the typed link facet.
newTypedLinkSchemaAndFacetName ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'typedLinkName'
  Prelude.Text ->
  TypedLinkSchemaAndFacetName
newTypedLinkSchemaAndFacetName
  pSchemaArn_
  pTypedLinkName_ =
    TypedLinkSchemaAndFacetName'
      { schemaArn =
          pSchemaArn_,
        typedLinkName = pTypedLinkName_
      }

-- | The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
typedLinkSchemaAndFacetName_schemaArn :: Lens.Lens' TypedLinkSchemaAndFacetName Prelude.Text
typedLinkSchemaAndFacetName_schemaArn = Lens.lens (\TypedLinkSchemaAndFacetName' {schemaArn} -> schemaArn) (\s@TypedLinkSchemaAndFacetName' {} a -> s {schemaArn = a} :: TypedLinkSchemaAndFacetName)

-- | The unique name of the typed link facet.
typedLinkSchemaAndFacetName_typedLinkName :: Lens.Lens' TypedLinkSchemaAndFacetName Prelude.Text
typedLinkSchemaAndFacetName_typedLinkName = Lens.lens (\TypedLinkSchemaAndFacetName' {typedLinkName} -> typedLinkName) (\s@TypedLinkSchemaAndFacetName' {} a -> s {typedLinkName = a} :: TypedLinkSchemaAndFacetName)

instance Data.FromJSON TypedLinkSchemaAndFacetName where
  parseJSON =
    Data.withObject
      "TypedLinkSchemaAndFacetName"
      ( \x ->
          TypedLinkSchemaAndFacetName'
            Prelude.<$> (x Data..: "SchemaArn")
            Prelude.<*> (x Data..: "TypedLinkName")
      )

instance Prelude.Hashable TypedLinkSchemaAndFacetName where
  hashWithSalt _salt TypedLinkSchemaAndFacetName' {..} =
    _salt
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` typedLinkName

instance Prelude.NFData TypedLinkSchemaAndFacetName where
  rnf TypedLinkSchemaAndFacetName' {..} =
    Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf typedLinkName

instance Data.ToJSON TypedLinkSchemaAndFacetName where
  toJSON TypedLinkSchemaAndFacetName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaArn" Data..= schemaArn),
            Prelude.Just
              ("TypedLinkName" Data..= typedLinkName)
          ]
      )
