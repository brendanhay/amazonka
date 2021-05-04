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
-- Module      : Network.AWS.ELBv2.Types.QueryStringKeyValuePair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.QueryStringKeyValuePair where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a key\/value pair.
--
-- /See:/ 'newQueryStringKeyValuePair' smart constructor.
data QueryStringKeyValuePair = QueryStringKeyValuePair'
  { -- | The key. You can omit the key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The key. You can omit the key.
queryStringKeyValuePair_key :: Lens.Lens' QueryStringKeyValuePair (Prelude.Maybe Prelude.Text)
queryStringKeyValuePair_key = Lens.lens (\QueryStringKeyValuePair' {key} -> key) (\s@QueryStringKeyValuePair' {} a -> s {key = a} :: QueryStringKeyValuePair)

-- | The value.
queryStringKeyValuePair_value :: Lens.Lens' QueryStringKeyValuePair (Prelude.Maybe Prelude.Text)
queryStringKeyValuePair_value = Lens.lens (\QueryStringKeyValuePair' {value} -> value) (\s@QueryStringKeyValuePair' {} a -> s {value = a} :: QueryStringKeyValuePair)

instance Prelude.FromXML QueryStringKeyValuePair where
  parseXML x =
    QueryStringKeyValuePair'
      Prelude.<$> (x Prelude..@? "Key")
      Prelude.<*> (x Prelude..@? "Value")

instance Prelude.Hashable QueryStringKeyValuePair

instance Prelude.NFData QueryStringKeyValuePair

instance Prelude.ToQuery QueryStringKeyValuePair where
  toQuery QueryStringKeyValuePair' {..} =
    Prelude.mconcat
      ["Key" Prelude.=: key, "Value" Prelude.=: value]
