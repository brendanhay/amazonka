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
-- Module      : Amazonka.EC2.Types.RequestIpamResourceTag
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.RequestIpamResourceTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | A tag on an IPAM resource.
--
-- /See:/ 'newRequestIpamResourceTag' smart constructor.
data RequestIpamResourceTag = RequestIpamResourceTag'
  { -- | The key of a tag assigned to the resource. Use this filter to find all
    -- resources assigned a tag with a specific key, regardless of the tag
    -- value.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value for the tag.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestIpamResourceTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'requestIpamResourceTag_key' - The key of a tag assigned to the resource. Use this filter to find all
-- resources assigned a tag with a specific key, regardless of the tag
-- value.
--
-- 'value', 'requestIpamResourceTag_value' - The value for the tag.
newRequestIpamResourceTag ::
  RequestIpamResourceTag
newRequestIpamResourceTag =
  RequestIpamResourceTag'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key of a tag assigned to the resource. Use this filter to find all
-- resources assigned a tag with a specific key, regardless of the tag
-- value.
requestIpamResourceTag_key :: Lens.Lens' RequestIpamResourceTag (Prelude.Maybe Prelude.Text)
requestIpamResourceTag_key = Lens.lens (\RequestIpamResourceTag' {key} -> key) (\s@RequestIpamResourceTag' {} a -> s {key = a} :: RequestIpamResourceTag)

-- | The value for the tag.
requestIpamResourceTag_value :: Lens.Lens' RequestIpamResourceTag (Prelude.Maybe Prelude.Text)
requestIpamResourceTag_value = Lens.lens (\RequestIpamResourceTag' {value} -> value) (\s@RequestIpamResourceTag' {} a -> s {value = a} :: RequestIpamResourceTag)

instance Prelude.Hashable RequestIpamResourceTag where
  hashWithSalt _salt RequestIpamResourceTag' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData RequestIpamResourceTag where
  rnf RequestIpamResourceTag' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToQuery RequestIpamResourceTag where
  toQuery RequestIpamResourceTag' {..} =
    Prelude.mconcat
      ["Key" Data.=: key, "Value" Data.=: value]
