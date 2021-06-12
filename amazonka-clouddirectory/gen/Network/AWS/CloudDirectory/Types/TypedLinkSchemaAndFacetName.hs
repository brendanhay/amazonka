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
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkSchemaAndFacetName where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identifies the schema Amazon Resource Name (ARN) and facet name for the
-- typed link.
--
-- /See:/ 'newTypedLinkSchemaAndFacetName' smart constructor.
data TypedLinkSchemaAndFacetName = TypedLinkSchemaAndFacetName'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Core.Text,
    -- | The unique name of the typed link facet.
    typedLinkName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'typedLinkName'
  Core.Text ->
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
typedLinkSchemaAndFacetName_schemaArn :: Lens.Lens' TypedLinkSchemaAndFacetName Core.Text
typedLinkSchemaAndFacetName_schemaArn = Lens.lens (\TypedLinkSchemaAndFacetName' {schemaArn} -> schemaArn) (\s@TypedLinkSchemaAndFacetName' {} a -> s {schemaArn = a} :: TypedLinkSchemaAndFacetName)

-- | The unique name of the typed link facet.
typedLinkSchemaAndFacetName_typedLinkName :: Lens.Lens' TypedLinkSchemaAndFacetName Core.Text
typedLinkSchemaAndFacetName_typedLinkName = Lens.lens (\TypedLinkSchemaAndFacetName' {typedLinkName} -> typedLinkName) (\s@TypedLinkSchemaAndFacetName' {} a -> s {typedLinkName = a} :: TypedLinkSchemaAndFacetName)

instance Core.FromJSON TypedLinkSchemaAndFacetName where
  parseJSON =
    Core.withObject
      "TypedLinkSchemaAndFacetName"
      ( \x ->
          TypedLinkSchemaAndFacetName'
            Core.<$> (x Core..: "SchemaArn")
            Core.<*> (x Core..: "TypedLinkName")
      )

instance Core.Hashable TypedLinkSchemaAndFacetName

instance Core.NFData TypedLinkSchemaAndFacetName

instance Core.ToJSON TypedLinkSchemaAndFacetName where
  toJSON TypedLinkSchemaAndFacetName' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SchemaArn" Core..= schemaArn),
            Core.Just ("TypedLinkName" Core..= typedLinkName)
          ]
      )
