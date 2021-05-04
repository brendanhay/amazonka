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
-- Module      : Network.AWS.MigrationHub.Types.ResourceAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ResourceAttribute where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types.ResourceAttributeType
import qualified Network.AWS.Prelude as Prelude

-- | Attribute associated with a resource.
--
-- Note the corresponding format required per type listed below:
--
-- [IPV4]
--     @x.x.x.x@
--
--     /where x is an integer in the range [0,255]/
--
-- [IPV6]
--     @y : y : y : y : y : y : y : y@
--
--     /where y is a hexadecimal between 0 and FFFF. [0, FFFF]/
--
-- [MAC_ADDRESS]
--     @^([0-9A-Fa-f]{2}[:-]){5}([0-9A-Fa-f]{2})$@
--
-- [FQDN]
--     @^[^\<>{}\\\\\\\\\/?,=\\\\p{Cntrl}]{1,256}$@
--
-- /See:/ 'newResourceAttribute' smart constructor.
data ResourceAttribute = ResourceAttribute'
  { -- | Type of resource.
    type' :: ResourceAttributeType,
    -- | Value of the resource type.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'resourceAttribute_type' - Type of resource.
--
-- 'value', 'resourceAttribute_value' - Value of the resource type.
newResourceAttribute ::
  -- | 'type''
  ResourceAttributeType ->
  -- | 'value'
  Prelude.Text ->
  ResourceAttribute
newResourceAttribute pType_ pValue_ =
  ResourceAttribute' {type' = pType_, value = pValue_}

-- | Type of resource.
resourceAttribute_type :: Lens.Lens' ResourceAttribute ResourceAttributeType
resourceAttribute_type = Lens.lens (\ResourceAttribute' {type'} -> type') (\s@ResourceAttribute' {} a -> s {type' = a} :: ResourceAttribute)

-- | Value of the resource type.
resourceAttribute_value :: Lens.Lens' ResourceAttribute Prelude.Text
resourceAttribute_value = Lens.lens (\ResourceAttribute' {value} -> value) (\s@ResourceAttribute' {} a -> s {value = a} :: ResourceAttribute)

instance Prelude.FromJSON ResourceAttribute where
  parseJSON =
    Prelude.withObject
      "ResourceAttribute"
      ( \x ->
          ResourceAttribute'
            Prelude.<$> (x Prelude..: "Type")
            Prelude.<*> (x Prelude..: "Value")
      )

instance Prelude.Hashable ResourceAttribute

instance Prelude.NFData ResourceAttribute

instance Prelude.ToJSON ResourceAttribute where
  toJSON ResourceAttribute' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Prelude..= type'),
            Prelude.Just ("Value" Prelude..= value)
          ]
      )
