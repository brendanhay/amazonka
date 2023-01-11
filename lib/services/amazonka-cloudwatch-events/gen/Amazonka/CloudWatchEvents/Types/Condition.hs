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
-- Module      : Amazonka.CloudWatchEvents.Types.Condition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.Condition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A JSON string which you can use to limit the event bus permissions you
-- are granting to only accounts that fulfill the condition. Currently, the
-- only supported condition is membership in a certain Amazon Web Services
-- organization. The string must contain @Type@, @Key@, and @Value@ fields.
-- The @Value@ field specifies the ID of the Amazon Web Services
-- organization. Following is an example value for @Condition@:
--
-- @\'{\"Type\" : \"StringEquals\", \"Key\": \"aws:PrincipalOrgID\", \"Value\": \"o-1234567890\"}\'@
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | Specifies the type of condition. Currently the only supported value is
    -- @StringEquals@.
    type' :: Prelude.Text,
    -- | Specifies the key for the condition. Currently the only supported key is
    -- @aws:PrincipalOrgID@.
    key :: Prelude.Text,
    -- | Specifies the value for the key. Currently, this must be the ID of the
    -- organization.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'condition_type' - Specifies the type of condition. Currently the only supported value is
-- @StringEquals@.
--
-- 'key', 'condition_key' - Specifies the key for the condition. Currently the only supported key is
-- @aws:PrincipalOrgID@.
--
-- 'value', 'condition_value' - Specifies the value for the key. Currently, this must be the ID of the
-- organization.
newCondition ::
  -- | 'type''
  Prelude.Text ->
  -- | 'key'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Condition
newCondition pType_ pKey_ pValue_ =
  Condition'
    { type' = pType_,
      key = pKey_,
      value = pValue_
    }

-- | Specifies the type of condition. Currently the only supported value is
-- @StringEquals@.
condition_type :: Lens.Lens' Condition Prelude.Text
condition_type = Lens.lens (\Condition' {type'} -> type') (\s@Condition' {} a -> s {type' = a} :: Condition)

-- | Specifies the key for the condition. Currently the only supported key is
-- @aws:PrincipalOrgID@.
condition_key :: Lens.Lens' Condition Prelude.Text
condition_key = Lens.lens (\Condition' {key} -> key) (\s@Condition' {} a -> s {key = a} :: Condition)

-- | Specifies the value for the key. Currently, this must be the ID of the
-- organization.
condition_value :: Lens.Lens' Condition Prelude.Text
condition_value = Lens.lens (\Condition' {value} -> value) (\s@Condition' {} a -> s {value = a} :: Condition)

instance Prelude.Hashable Condition where
  hashWithSalt _salt Condition' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Condition where
  rnf Condition' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Condition where
  toJSON Condition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Value" Data..= value)
          ]
      )
