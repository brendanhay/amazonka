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
-- Module      : Network.AWS.ELBv2.Types.HostHeaderConditionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.HostHeaderConditionConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a host header condition.
--
-- /See:/ 'newHostHeaderConditionConfig' smart constructor.
data HostHeaderConditionConfig = HostHeaderConditionConfig'
  { -- | One or more host names. The maximum size of each name is 128 characters.
    -- The comparison is case insensitive. The following wildcard characters
    -- are supported: * (matches 0 or more characters) and ? (matches exactly 1
    -- character).
    --
    -- If you specify multiple strings, the condition is satisfied if one of
    -- the strings matches the host name.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HostHeaderConditionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'hostHeaderConditionConfig_values' - One or more host names. The maximum size of each name is 128 characters.
-- The comparison is case insensitive. The following wildcard characters
-- are supported: * (matches 0 or more characters) and ? (matches exactly 1
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

-- | One or more host names. The maximum size of each name is 128 characters.
-- The comparison is case insensitive. The following wildcard characters
-- are supported: * (matches 0 or more characters) and ? (matches exactly 1
-- character).
--
-- If you specify multiple strings, the condition is satisfied if one of
-- the strings matches the host name.
hostHeaderConditionConfig_values :: Lens.Lens' HostHeaderConditionConfig (Prelude.Maybe [Prelude.Text])
hostHeaderConditionConfig_values = Lens.lens (\HostHeaderConditionConfig' {values} -> values) (\s@HostHeaderConditionConfig' {} a -> s {values = a} :: HostHeaderConditionConfig) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML HostHeaderConditionConfig where
  parseXML x =
    HostHeaderConditionConfig'
      Prelude.<$> ( x Prelude..@? "Values" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable HostHeaderConditionConfig

instance Prelude.NFData HostHeaderConditionConfig

instance Prelude.ToQuery HostHeaderConditionConfig where
  toQuery HostHeaderConditionConfig' {..} =
    Prelude.mconcat
      [ "Values"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> values)
      ]
