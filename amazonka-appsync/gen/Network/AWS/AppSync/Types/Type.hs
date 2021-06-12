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
-- Module      : Network.AWS.AppSync.Types.Type
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.Type where

import Network.AWS.AppSync.Types.TypeDefinitionFormat
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a type.
--
-- /See:/ 'newType' smart constructor.
data Type = Type'
  { -- | The type format: SDL or JSON.
    format :: Core.Maybe TypeDefinitionFormat,
    -- | The type ARN.
    arn :: Core.Maybe Core.Text,
    -- | The type name.
    name :: Core.Maybe Core.Text,
    -- | The type description.
    description :: Core.Maybe Core.Text,
    -- | The type definition.
    definition :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Type' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'type_format' - The type format: SDL or JSON.
--
-- 'arn', 'type_arn' - The type ARN.
--
-- 'name', 'type_name' - The type name.
--
-- 'description', 'type_description' - The type description.
--
-- 'definition', 'type_definition' - The type definition.
newType ::
  Type
newType =
  Type'
    { format = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      definition = Core.Nothing
    }

-- | The type format: SDL or JSON.
type_format :: Lens.Lens' Type (Core.Maybe TypeDefinitionFormat)
type_format = Lens.lens (\Type' {format} -> format) (\s@Type' {} a -> s {format = a} :: Type)

-- | The type ARN.
type_arn :: Lens.Lens' Type (Core.Maybe Core.Text)
type_arn = Lens.lens (\Type' {arn} -> arn) (\s@Type' {} a -> s {arn = a} :: Type)

-- | The type name.
type_name :: Lens.Lens' Type (Core.Maybe Core.Text)
type_name = Lens.lens (\Type' {name} -> name) (\s@Type' {} a -> s {name = a} :: Type)

-- | The type description.
type_description :: Lens.Lens' Type (Core.Maybe Core.Text)
type_description = Lens.lens (\Type' {description} -> description) (\s@Type' {} a -> s {description = a} :: Type)

-- | The type definition.
type_definition :: Lens.Lens' Type (Core.Maybe Core.Text)
type_definition = Lens.lens (\Type' {definition} -> definition) (\s@Type' {} a -> s {definition = a} :: Type)

instance Core.FromJSON Type where
  parseJSON =
    Core.withObject
      "Type"
      ( \x ->
          Type'
            Core.<$> (x Core..:? "format")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "definition")
      )

instance Core.Hashable Type

instance Core.NFData Type
