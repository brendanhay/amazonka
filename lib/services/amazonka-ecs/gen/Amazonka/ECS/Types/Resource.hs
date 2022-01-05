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
-- Module      : Amazonka.ECS.Types.Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the resources available for a container instance.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | When the @stringSetValue@ type is set, the value of the resource must be
    -- a string type.
    stringSetValue :: Prelude.Maybe [Prelude.Text],
    -- | When the @integerValue@ type is set, the value of the resource must be
    -- an integer.
    integerValue :: Prelude.Maybe Prelude.Int,
    -- | When the @doubleValue@ type is set, the value of the resource must be a
    -- double precision floating-point type.
    doubleValue :: Prelude.Maybe Prelude.Double,
    -- | When the @longValue@ type is set, the value of the resource must be an
    -- extended precision floating-point type.
    longValue :: Prelude.Maybe Prelude.Integer,
    -- | The name of the resource, such as @CPU@, @MEMORY@, @PORTS@, @PORTS_UDP@,
    -- or a user-defined resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the resource, such as @INTEGER@, @DOUBLE@, @LONG@, or
    -- @STRINGSET@.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'integerValue', 'resource_integerValue' - When the @integerValue@ type is set, the value of the resource must be
-- an integer.
--
-- 'doubleValue', 'resource_doubleValue' - When the @doubleValue@ type is set, the value of the resource must be a
-- double precision floating-point type.
--
-- 'longValue', 'resource_longValue' - When the @longValue@ type is set, the value of the resource must be an
-- extended precision floating-point type.
--
-- 'name', 'resource_name' - The name of the resource, such as @CPU@, @MEMORY@, @PORTS@, @PORTS_UDP@,
-- or a user-defined resource.
--
-- 'type'', 'resource_type' - The type of the resource, such as @INTEGER@, @DOUBLE@, @LONG@, or
-- @STRINGSET@.
newResource ::
  Resource
newResource =
  Resource'
    { stringSetValue = Prelude.Nothing,
      integerValue = Prelude.Nothing,
      doubleValue = Prelude.Nothing,
      longValue = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | When the @stringSetValue@ type is set, the value of the resource must be
-- a string type.
resource_stringSetValue :: Lens.Lens' Resource (Prelude.Maybe [Prelude.Text])
resource_stringSetValue = Lens.lens (\Resource' {stringSetValue} -> stringSetValue) (\s@Resource' {} a -> s {stringSetValue = a} :: Resource) Prelude.. Lens.mapping Lens.coerced

-- | When the @integerValue@ type is set, the value of the resource must be
-- an integer.
resource_integerValue :: Lens.Lens' Resource (Prelude.Maybe Prelude.Int)
resource_integerValue = Lens.lens (\Resource' {integerValue} -> integerValue) (\s@Resource' {} a -> s {integerValue = a} :: Resource)

-- | When the @doubleValue@ type is set, the value of the resource must be a
-- double precision floating-point type.
resource_doubleValue :: Lens.Lens' Resource (Prelude.Maybe Prelude.Double)
resource_doubleValue = Lens.lens (\Resource' {doubleValue} -> doubleValue) (\s@Resource' {} a -> s {doubleValue = a} :: Resource)

-- | When the @longValue@ type is set, the value of the resource must be an
-- extended precision floating-point type.
resource_longValue :: Lens.Lens' Resource (Prelude.Maybe Prelude.Integer)
resource_longValue = Lens.lens (\Resource' {longValue} -> longValue) (\s@Resource' {} a -> s {longValue = a} :: Resource)

-- | The name of the resource, such as @CPU@, @MEMORY@, @PORTS@, @PORTS_UDP@,
-- or a user-defined resource.
resource_name :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_name = Lens.lens (\Resource' {name} -> name) (\s@Resource' {} a -> s {name = a} :: Resource)

-- | The type of the resource, such as @INTEGER@, @DOUBLE@, @LONG@, or
-- @STRINGSET@.
resource_type :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

instance Core.FromJSON Resource where
  parseJSON =
    Core.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> (x Core..:? "stringSetValue" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "integerValue")
            Prelude.<*> (x Core..:? "doubleValue")
            Prelude.<*> (x Core..:? "longValue")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "type")
      )

instance Prelude.Hashable Resource where
  hashWithSalt _salt Resource' {..} =
    _salt `Prelude.hashWithSalt` stringSetValue
      `Prelude.hashWithSalt` integerValue
      `Prelude.hashWithSalt` doubleValue
      `Prelude.hashWithSalt` longValue
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData Resource where
  rnf Resource' {..} =
    Prelude.rnf stringSetValue
      `Prelude.seq` Prelude.rnf integerValue
      `Prelude.seq` Prelude.rnf doubleValue
      `Prelude.seq` Prelude.rnf longValue
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON Resource where
  toJSON Resource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("stringSetValue" Core..=)
              Prelude.<$> stringSetValue,
            ("integerValue" Core..=) Prelude.<$> integerValue,
            ("doubleValue" Core..=) Prelude.<$> doubleValue,
            ("longValue" Core..=) Prelude.<$> longValue,
            ("name" Core..=) Prelude.<$> name,
            ("type" Core..=) Prelude.<$> type'
          ]
      )
