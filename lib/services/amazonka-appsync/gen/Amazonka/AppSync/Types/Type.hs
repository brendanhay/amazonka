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
-- Module      : Amazonka.AppSync.Types.Type
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.Type where

import Amazonka.AppSync.Types.TypeDefinitionFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a type.
--
-- /See:/ 'newType' smart constructor.
data Type = Type'
  { -- | The type name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type format: SDL or JSON.
    format :: Prelude.Maybe TypeDefinitionFormat,
    -- | The type Amazon Resource Name (ARN).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type definition.
    definition :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Type' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'type_name' - The type name.
--
-- 'format', 'type_format' - The type format: SDL or JSON.
--
-- 'arn', 'type_arn' - The type Amazon Resource Name (ARN).
--
-- 'description', 'type_description' - The type description.
--
-- 'definition', 'type_definition' - The type definition.
newType ::
  Type
newType =
  Type'
    { name = Prelude.Nothing,
      format = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      definition = Prelude.Nothing
    }

-- | The type name.
type_name :: Lens.Lens' Type (Prelude.Maybe Prelude.Text)
type_name = Lens.lens (\Type' {name} -> name) (\s@Type' {} a -> s {name = a} :: Type)

-- | The type format: SDL or JSON.
type_format :: Lens.Lens' Type (Prelude.Maybe TypeDefinitionFormat)
type_format = Lens.lens (\Type' {format} -> format) (\s@Type' {} a -> s {format = a} :: Type)

-- | The type Amazon Resource Name (ARN).
type_arn :: Lens.Lens' Type (Prelude.Maybe Prelude.Text)
type_arn = Lens.lens (\Type' {arn} -> arn) (\s@Type' {} a -> s {arn = a} :: Type)

-- | The type description.
type_description :: Lens.Lens' Type (Prelude.Maybe Prelude.Text)
type_description = Lens.lens (\Type' {description} -> description) (\s@Type' {} a -> s {description = a} :: Type)

-- | The type definition.
type_definition :: Lens.Lens' Type (Prelude.Maybe Prelude.Text)
type_definition = Lens.lens (\Type' {definition} -> definition) (\s@Type' {} a -> s {definition = a} :: Type)

instance Core.FromJSON Type where
  parseJSON =
    Core.withObject
      "Type"
      ( \x ->
          Type'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "format")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "definition")
      )

instance Prelude.Hashable Type where
  hashWithSalt _salt Type' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` definition

instance Prelude.NFData Type where
  rnf Type' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf definition
