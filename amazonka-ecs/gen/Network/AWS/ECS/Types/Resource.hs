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
-- Module      : Network.AWS.ECS.Types.Resource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Resource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the resources available for a container instance.
--
-- /See:/ 'newResource' smart constructor.
data Resource = Resource'
  { -- | When the @stringSetValue@ type is set, the value of the resource must be
    -- a string type.
    stringSetValue :: Prelude.Maybe [Prelude.Text],
    -- | When the @doubleValue@ type is set, the value of the resource must be a
    -- double precision floating-point type.
    doubleValue :: Prelude.Maybe Prelude.Double,
    -- | The name of the resource, such as @CPU@, @MEMORY@, @PORTS@, @PORTS_UDP@,
    -- or a user-defined resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | When the @longValue@ type is set, the value of the resource must be an
    -- extended precision floating-point type.
    longValue :: Prelude.Maybe Prelude.Integer,
    -- | The type of the resource, such as @INTEGER@, @DOUBLE@, @LONG@, or
    -- @STRINGSET@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | When the @integerValue@ type is set, the value of the resource must be
    -- an integer.
    integerValue :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { stringSetValue = Prelude.Nothing,
      doubleValue = Prelude.Nothing,
      name = Prelude.Nothing,
      longValue = Prelude.Nothing,
      type' = Prelude.Nothing,
      integerValue = Prelude.Nothing
    }

-- | When the @stringSetValue@ type is set, the value of the resource must be
-- a string type.
resource_stringSetValue :: Lens.Lens' Resource (Prelude.Maybe [Prelude.Text])
resource_stringSetValue = Lens.lens (\Resource' {stringSetValue} -> stringSetValue) (\s@Resource' {} a -> s {stringSetValue = a} :: Resource) Prelude.. Lens.mapping Prelude._Coerce

-- | When the @doubleValue@ type is set, the value of the resource must be a
-- double precision floating-point type.
resource_doubleValue :: Lens.Lens' Resource (Prelude.Maybe Prelude.Double)
resource_doubleValue = Lens.lens (\Resource' {doubleValue} -> doubleValue) (\s@Resource' {} a -> s {doubleValue = a} :: Resource)

-- | The name of the resource, such as @CPU@, @MEMORY@, @PORTS@, @PORTS_UDP@,
-- or a user-defined resource.
resource_name :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_name = Lens.lens (\Resource' {name} -> name) (\s@Resource' {} a -> s {name = a} :: Resource)

-- | When the @longValue@ type is set, the value of the resource must be an
-- extended precision floating-point type.
resource_longValue :: Lens.Lens' Resource (Prelude.Maybe Prelude.Integer)
resource_longValue = Lens.lens (\Resource' {longValue} -> longValue) (\s@Resource' {} a -> s {longValue = a} :: Resource)

-- | The type of the resource, such as @INTEGER@, @DOUBLE@, @LONG@, or
-- @STRINGSET@.
resource_type :: Lens.Lens' Resource (Prelude.Maybe Prelude.Text)
resource_type = Lens.lens (\Resource' {type'} -> type') (\s@Resource' {} a -> s {type' = a} :: Resource)

-- | When the @integerValue@ type is set, the value of the resource must be
-- an integer.
resource_integerValue :: Lens.Lens' Resource (Prelude.Maybe Prelude.Int)
resource_integerValue = Lens.lens (\Resource' {integerValue} -> integerValue) (\s@Resource' {} a -> s {integerValue = a} :: Resource)

instance Prelude.FromJSON Resource where
  parseJSON =
    Prelude.withObject
      "Resource"
      ( \x ->
          Resource'
            Prelude.<$> ( x Prelude..:? "stringSetValue"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "doubleValue")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "longValue")
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..:? "integerValue")
      )

instance Prelude.Hashable Resource

instance Prelude.NFData Resource

instance Prelude.ToJSON Resource where
  toJSON Resource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("stringSetValue" Prelude..=)
              Prelude.<$> stringSetValue,
            ("doubleValue" Prelude..=) Prelude.<$> doubleValue,
            ("name" Prelude..=) Prelude.<$> name,
            ("longValue" Prelude..=) Prelude.<$> longValue,
            ("type" Prelude..=) Prelude.<$> type',
            ("integerValue" Prelude..=)
              Prelude.<$> integerValue
          ]
      )
