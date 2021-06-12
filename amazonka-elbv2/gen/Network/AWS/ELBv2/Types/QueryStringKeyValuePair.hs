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
-- Module      : Network.AWS.ELBv2.Types.QueryStringKeyValuePair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.QueryStringKeyValuePair where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a key\/value pair.
--
-- /See:/ 'newQueryStringKeyValuePair' smart constructor.
data QueryStringKeyValuePair = QueryStringKeyValuePair'
  { -- | The key. You can omit the key.
    key :: Core.Maybe Core.Text,
    -- | The value.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueryStringKeyValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'queryStringKeyValuePair_key' - The key. You can omit the key.
--
-- 'value', 'queryStringKeyValuePair_value' - The value.
newQueryStringKeyValuePair ::
  QueryStringKeyValuePair
newQueryStringKeyValuePair =
  QueryStringKeyValuePair'
    { key = Core.Nothing,
      value = Core.Nothing
    }

-- | The key. You can omit the key.
queryStringKeyValuePair_key :: Lens.Lens' QueryStringKeyValuePair (Core.Maybe Core.Text)
queryStringKeyValuePair_key = Lens.lens (\QueryStringKeyValuePair' {key} -> key) (\s@QueryStringKeyValuePair' {} a -> s {key = a} :: QueryStringKeyValuePair)

-- | The value.
queryStringKeyValuePair_value :: Lens.Lens' QueryStringKeyValuePair (Core.Maybe Core.Text)
queryStringKeyValuePair_value = Lens.lens (\QueryStringKeyValuePair' {value} -> value) (\s@QueryStringKeyValuePair' {} a -> s {value = a} :: QueryStringKeyValuePair)

instance Core.FromXML QueryStringKeyValuePair where
  parseXML x =
    QueryStringKeyValuePair'
      Core.<$> (x Core..@? "Key") Core.<*> (x Core..@? "Value")

instance Core.Hashable QueryStringKeyValuePair

instance Core.NFData QueryStringKeyValuePair

instance Core.ToQuery QueryStringKeyValuePair where
  toQuery QueryStringKeyValuePair' {..} =
    Core.mconcat
      ["Key" Core.=: key, "Value" Core.=: value]
