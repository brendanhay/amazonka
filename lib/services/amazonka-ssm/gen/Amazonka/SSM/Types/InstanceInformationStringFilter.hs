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
-- Module      : Amazonka.SSM.Types.InstanceInformationStringFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstanceInformationStringFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The filters to describe or get information about your managed nodes.
--
-- /See:/ 'newInstanceInformationStringFilter' smart constructor.
data InstanceInformationStringFilter = InstanceInformationStringFilter'
  { -- | The filter key name to describe your managed nodes. For example:
    --
    -- \"InstanceIds\" | \"AgentVersion\" | \"PingStatus\" | \"PlatformTypes\"
    -- | \"ActivationIds\" | \"IamRole\" | \"ResourceType\" |
    -- \"AssociationStatus\" | \"tag-key\" | \"tag:@{keyname}@
    --
    -- @Tag Key@ isn\'t a valid filter. You must specify either @tag-key@ or
    -- @tag:{keyname}@ and a string. Here are some valid examples: @tag-key@,
    -- @tag:123@, @tag:al!@, @tag:Windows@. Here are some /invalid/ examples:
    -- @tag-keys@, @Tag Key@, @tag:@, @tagKey@, @abc:keyname@.
    key :: Prelude.Text,
    -- | The filter values.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceInformationStringFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'instanceInformationStringFilter_key' - The filter key name to describe your managed nodes. For example:
--
-- \"InstanceIds\" | \"AgentVersion\" | \"PingStatus\" | \"PlatformTypes\"
-- | \"ActivationIds\" | \"IamRole\" | \"ResourceType\" |
-- \"AssociationStatus\" | \"tag-key\" | \"tag:@{keyname}@
--
-- @Tag Key@ isn\'t a valid filter. You must specify either @tag-key@ or
-- @tag:{keyname}@ and a string. Here are some valid examples: @tag-key@,
-- @tag:123@, @tag:al!@, @tag:Windows@. Here are some /invalid/ examples:
-- @tag-keys@, @Tag Key@, @tag:@, @tagKey@, @abc:keyname@.
--
-- 'values', 'instanceInformationStringFilter_values' - The filter values.
newInstanceInformationStringFilter ::
  -- | 'key'
  Prelude.Text ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  InstanceInformationStringFilter
newInstanceInformationStringFilter pKey_ pValues_ =
  InstanceInformationStringFilter'
    { key = pKey_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The filter key name to describe your managed nodes. For example:
--
-- \"InstanceIds\" | \"AgentVersion\" | \"PingStatus\" | \"PlatformTypes\"
-- | \"ActivationIds\" | \"IamRole\" | \"ResourceType\" |
-- \"AssociationStatus\" | \"tag-key\" | \"tag:@{keyname}@
--
-- @Tag Key@ isn\'t a valid filter. You must specify either @tag-key@ or
-- @tag:{keyname}@ and a string. Here are some valid examples: @tag-key@,
-- @tag:123@, @tag:al!@, @tag:Windows@. Here are some /invalid/ examples:
-- @tag-keys@, @Tag Key@, @tag:@, @tagKey@, @abc:keyname@.
instanceInformationStringFilter_key :: Lens.Lens' InstanceInformationStringFilter Prelude.Text
instanceInformationStringFilter_key = Lens.lens (\InstanceInformationStringFilter' {key} -> key) (\s@InstanceInformationStringFilter' {} a -> s {key = a} :: InstanceInformationStringFilter)

-- | The filter values.
instanceInformationStringFilter_values :: Lens.Lens' InstanceInformationStringFilter (Prelude.NonEmpty Prelude.Text)
instanceInformationStringFilter_values = Lens.lens (\InstanceInformationStringFilter' {values} -> values) (\s@InstanceInformationStringFilter' {} a -> s {values = a} :: InstanceInformationStringFilter) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    InstanceInformationStringFilter
  where
  hashWithSalt
    _salt
    InstanceInformationStringFilter' {..} =
      _salt `Prelude.hashWithSalt` key
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    InstanceInformationStringFilter
  where
  rnf InstanceInformationStringFilter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Core.ToJSON InstanceInformationStringFilter where
  toJSON InstanceInformationStringFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Values" Core..= values)
          ]
      )
