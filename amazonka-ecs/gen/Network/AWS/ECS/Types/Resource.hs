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
-- Module      : Network.AWS.ECS.Types.Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Resource where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the resources available for a container instance.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | When the @stringSetValue@ type is set, the value of the resource must be
    -- a string type.
    stringSetValue :: Core.Maybe [Core.Text],
    -- | When the @doubleValue@ type is set, the value of the resource must be a
    -- double precision floating-point type.
    doubleValue :: Core.Maybe Core.Double,
    -- | The name of the resource, such as @CPU@, @MEMORY@, @PORTS@, @PORTS_UDP@,
    -- or a user-defined resource.
    name :: Core.Maybe Core.Text,
    -- | When the @longValue@ type is set, the value of the resource must be an
    -- extended precision floating-point type.
    longValue :: Core.Maybe Core.Integer,
    -- | The type of the resource, such as @INTEGER@, @DOUBLE@, @LONG@, or
    -- @STRINGSET@.
    type' :: Core.Maybe Core.Text,
    -- | When the @integerValue@ type is set, the value of the resource must be
    -- an integer.
    integerValue :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stringSetValue', 'resource_stringSetValue' - When the @stringSetValue@ type is set, the value of the resource must be
-- a string type.
--
-- 'doubleValue', 'resource_doubleValue' - When the @doubleValue@ type is set, the value of the resource must be a
-- double precision floating-point type.
--
-- 'name', 'resource_name' - The name of the resource, such as @CPU@, @MEMORY@, @PORTS@, @PORTS_UDP@,
-- or a user-defined resource.
--
-- 'longValue', 'resource_longValue' - When the @longValue@ type is set, the value of the resource must be an
-- extended precision floating-point type.
--
-- 'type'', 'resource_type' - The type of the resource, such as @INTEGER@, @DOUBLE@, @LONG@, or
-- @STRINGSET@.
--
-- 'integerValue', 'resource_integerValue' - When the @integerValue@ type is set, the value of the resource must be
-- an integer.
newResource ::
  Resource
newResource =
  Resource'
    { stringSetValue = Core.Nothing,
      doubleValue = Core.Nothing,
      name = Core.Nothing,
      longValue = Core.Nothing,
      type' = Core.Nothing,
      integerValue = Core.Nothing
    }

-- | When the @stringSetValue@ type is set, the value of the resource must be
-- a string type.
resource_stringSetValue :: Lens.Lens' Resource (Core.Maybe [Core.Text])
resource_stringSetValue = Lens.lens (\Resource' {stringSetValue} -> stringSetValue) (\s@Resource' {} a -> s {stringSetValue = a} :: Resource) Core.. Lens.mapping Lens._Coerce

-- | When the @doubleValue@ type is set, the value of the resource must be a
-- double precision floating-point type.
resource_doubleValue :: Lens.Lens' Resource (Core.Maybe Core.Double)
resource_doubleValue = Lens.lens (\Resource' {doubleValue} -> doubleValue) (\s@Resource' {} a -> s {doubleValue = a} :: Resource)

-- | The name of the resource, such as @CPU@, @MEMORY@, @PORTS@, @PORTS_UDP@,
-- or a user-defined resource.
resource_name :: Lens.Lens' Resource (Core.Maybe Core.Text)
resource_name = Lens.lens (\Resource' {name} -> name) (\s@Resource' {} a -> s {name = a} :: Resource)

-- | When the @longValue@ type is set, the value of the resource must be an
-- extended precision floating-point type.
resource_longValue :: Lens.Lens' Resource (Core.Maybe Core.Integer)
resource_longValue = Lens.lens (\Resource' {longValue} -> longValue) (\s@Resource' {} a -> s {longValue = a} :: Resource)

-- | The type of the resource, such as @INTEGER@, @DOUBLE@, @LONG@, or
-- @STRINGSET@.
resource_type :: Lens.Lens' Resource (Core.Maybe Core.Text)
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

-- | When the @integerValue@ type is set, the value of the resource must be
-- an integer.
resource_integerValue :: Lens.Lens' Resource (Core.Maybe Core.Int)
resource_integerValue = Lens.lens (\Resource' {integerValue} -> integerValue) (\s@Resource' {} a -> s {integerValue = a} :: Resource)

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Core.<$> (x Core..:? "stringSetValue" Core..!= Core.mempty)
            Core.<*> (x Core..:? "doubleValue")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "longValue")
            Core.<*> (x Core..:? "type")
            Core.<*> (x Core..:? "integerValue")
      )

instance Core.Hashable Resource

instance Core.NFData Resource

instance Core.ToJSON Resource where
  toJSON Resource' {..} =
    Core.object
      ( Core.catMaybes
          [ ("stringSetValue" Core..=) Core.<$> stringSetValue,
            ("doubleValue" Core..=) Core.<$> doubleValue,
            ("name" Core..=) Core.<$> name,
            ("longValue" Core..=) Core.<$> longValue,
            ("type" Core..=) Core.<$> type',
            ("integerValue" Core..=) Core.<$> integerValue
          ]
      )
