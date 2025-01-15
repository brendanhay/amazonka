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
-- Module      : Amazonka.ELBV2.Types.HostHeaderConditionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.HostHeaderConditionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a host header condition.
--
-- /See:/ 'newHostHeaderConditionConfig' smart constructor.
data HostHeaderConditionConfig = HostHeaderConditionConfig'
  { -- | The host names. The maximum size of each name is 128 characters. The
    -- comparison is case insensitive. The following wildcard characters are
    -- supported: * (matches 0 or more characters) and ? (matches exactly 1
    -- character).
    --
    -- If you specify multiple strings, the condition is satisfied if one of
    -- the strings matches the host name.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HostHeaderConditionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'hostHeaderConditionConfig_values' - The host names. The maximum size of each name is 128 characters. The
-- comparison is case insensitive. The following wildcard characters are
-- supported: * (matches 0 or more characters) and ? (matches exactly 1
-- character).
--
-- If you specify multiple strings, the condition is satisfied if one of
-- the strings matches the host name.
newHostHeaderConditionConfig ::
  HostHeaderConditionConfig
newHostHeaderConditionConfig =
  HostHeaderConditionConfig'
    { values =
        Prelude.Nothing
    }

-- | The host names. The maximum size of each name is 128 characters. The
-- comparison is case insensitive. The following wildcard characters are
-- supported: * (matches 0 or more characters) and ? (matches exactly 1
-- character).
--
-- If you specify multiple strings, the condition is satisfied if one of
-- the strings matches the host name.
hostHeaderConditionConfig_values :: Lens.Lens' HostHeaderConditionConfig (Prelude.Maybe [Prelude.Text])
hostHeaderConditionConfig_values = Lens.lens (\HostHeaderConditionConfig' {values} -> values) (\s@HostHeaderConditionConfig' {} a -> s {values = a} :: HostHeaderConditionConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML HostHeaderConditionConfig where
  parseXML x =
    HostHeaderConditionConfig'
      Prelude.<$> ( x Data..@? "Values" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable HostHeaderConditionConfig where
  hashWithSalt _salt HostHeaderConditionConfig' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData HostHeaderConditionConfig where
  rnf HostHeaderConditionConfig' {..} =
    Prelude.rnf values

instance Data.ToQuery HostHeaderConditionConfig where
  toQuery HostHeaderConditionConfig' {..} =
    Prelude.mconcat
      [ "Values"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> values)
      ]
