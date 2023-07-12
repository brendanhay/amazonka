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
-- Module      : Amazonka.EC2.Types.IpamResourceTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamResourceTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The key\/value combination of a tag assigned to the resource. Use the
-- tag key in the filter name and the tag value as the filter value. For
-- example, to find all resources that have a tag with the key @Owner@ and
-- the value @TeamA@, specify @tag:Owner@ for the filter name and @TeamA@
-- for the filter value.
--
-- /See:/ 'newIpamResourceTag' smart constructor.
data IpamResourceTag = IpamResourceTag'
  { -- | The key of a tag assigned to the resource. Use this filter to find all
    -- resources assigned a tag with a specific key, regardless of the tag
    -- value.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the tag.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamResourceTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'ipamResourceTag_key' - The key of a tag assigned to the resource. Use this filter to find all
-- resources assigned a tag with a specific key, regardless of the tag
-- value.
--
-- 'value', 'ipamResourceTag_value' - The value of the tag.
newIpamResourceTag ::
  IpamResourceTag
newIpamResourceTag =
  IpamResourceTag'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key of a tag assigned to the resource. Use this filter to find all
-- resources assigned a tag with a specific key, regardless of the tag
-- value.
ipamResourceTag_key :: Lens.Lens' IpamResourceTag (Prelude.Maybe Prelude.Text)
ipamResourceTag_key = Lens.lens (\IpamResourceTag' {key} -> key) (\s@IpamResourceTag' {} a -> s {key = a} :: IpamResourceTag)

-- | The value of the tag.
ipamResourceTag_value :: Lens.Lens' IpamResourceTag (Prelude.Maybe Prelude.Text)
ipamResourceTag_value = Lens.lens (\IpamResourceTag' {value} -> value) (\s@IpamResourceTag' {} a -> s {value = a} :: IpamResourceTag)

instance Data.FromXML IpamResourceTag where
  parseXML x =
    IpamResourceTag'
      Prelude.<$> (x Data..@? "key")
      Prelude.<*> (x Data..@? "value")

instance Prelude.Hashable IpamResourceTag where
  hashWithSalt _salt IpamResourceTag' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData IpamResourceTag where
  rnf IpamResourceTag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value
