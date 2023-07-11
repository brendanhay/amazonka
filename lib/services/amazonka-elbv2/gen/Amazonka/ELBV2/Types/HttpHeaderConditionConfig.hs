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
-- Module      : Amazonka.ELBV2.Types.HttpHeaderConditionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.HttpHeaderConditionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an HTTP header condition.
--
-- There is a set of standard HTTP header fields. You can also define
-- custom HTTP header fields.
--
-- /See:/ 'newHttpHeaderConditionConfig' smart constructor.
data HttpHeaderConditionConfig = HttpHeaderConditionConfig'
  { -- | The name of the HTTP header field. The maximum size is 40 characters.
    -- The header name is case insensitive. The allowed characters are
    -- specified by RFC 7230. Wildcards are not supported.
    --
    -- You can\'t use an HTTP header condition to specify the host header. Use
    -- HostHeaderConditionConfig to specify a host header condition.
    httpHeaderName :: Prelude.Maybe Prelude.Text,
    -- | The strings to compare against the value of the HTTP header. The maximum
    -- size of each string is 128 characters. The comparison strings are case
    -- insensitive. The following wildcard characters are supported: * (matches
    -- 0 or more characters) and ? (matches exactly 1 character).
    --
    -- If the same header appears multiple times in the request, we search them
    -- in order until a match is found.
    --
    -- If you specify multiple strings, the condition is satisfied if one of
    -- the strings matches the value of the HTTP header. To require that all of
    -- the strings are a match, create one condition per string.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpHeaderConditionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpHeaderName', 'httpHeaderConditionConfig_httpHeaderName' - The name of the HTTP header field. The maximum size is 40 characters.
-- The header name is case insensitive. The allowed characters are
-- specified by RFC 7230. Wildcards are not supported.
--
-- You can\'t use an HTTP header condition to specify the host header. Use
-- HostHeaderConditionConfig to specify a host header condition.
--
-- 'values', 'httpHeaderConditionConfig_values' - The strings to compare against the value of the HTTP header. The maximum
-- size of each string is 128 characters. The comparison strings are case
-- insensitive. The following wildcard characters are supported: * (matches
-- 0 or more characters) and ? (matches exactly 1 character).
--
-- If the same header appears multiple times in the request, we search them
-- in order until a match is found.
--
-- If you specify multiple strings, the condition is satisfied if one of
-- the strings matches the value of the HTTP header. To require that all of
-- the strings are a match, create one condition per string.
newHttpHeaderConditionConfig ::
  HttpHeaderConditionConfig
newHttpHeaderConditionConfig =
  HttpHeaderConditionConfig'
    { httpHeaderName =
        Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The name of the HTTP header field. The maximum size is 40 characters.
-- The header name is case insensitive. The allowed characters are
-- specified by RFC 7230. Wildcards are not supported.
--
-- You can\'t use an HTTP header condition to specify the host header. Use
-- HostHeaderConditionConfig to specify a host header condition.
httpHeaderConditionConfig_httpHeaderName :: Lens.Lens' HttpHeaderConditionConfig (Prelude.Maybe Prelude.Text)
httpHeaderConditionConfig_httpHeaderName = Lens.lens (\HttpHeaderConditionConfig' {httpHeaderName} -> httpHeaderName) (\s@HttpHeaderConditionConfig' {} a -> s {httpHeaderName = a} :: HttpHeaderConditionConfig)

-- | The strings to compare against the value of the HTTP header. The maximum
-- size of each string is 128 characters. The comparison strings are case
-- insensitive. The following wildcard characters are supported: * (matches
-- 0 or more characters) and ? (matches exactly 1 character).
--
-- If the same header appears multiple times in the request, we search them
-- in order until a match is found.
--
-- If you specify multiple strings, the condition is satisfied if one of
-- the strings matches the value of the HTTP header. To require that all of
-- the strings are a match, create one condition per string.
httpHeaderConditionConfig_values :: Lens.Lens' HttpHeaderConditionConfig (Prelude.Maybe [Prelude.Text])
httpHeaderConditionConfig_values = Lens.lens (\HttpHeaderConditionConfig' {values} -> values) (\s@HttpHeaderConditionConfig' {} a -> s {values = a} :: HttpHeaderConditionConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML HttpHeaderConditionConfig where
  parseXML x =
    HttpHeaderConditionConfig'
      Prelude.<$> (x Data..@? "HttpHeaderName")
      Prelude.<*> ( x
                      Data..@? "Values"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable HttpHeaderConditionConfig where
  hashWithSalt _salt HttpHeaderConditionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` httpHeaderName
      `Prelude.hashWithSalt` values

instance Prelude.NFData HttpHeaderConditionConfig where
  rnf HttpHeaderConditionConfig' {..} =
    Prelude.rnf httpHeaderName
      `Prelude.seq` Prelude.rnf values

instance Data.ToQuery HttpHeaderConditionConfig where
  toQuery HttpHeaderConditionConfig' {..} =
    Prelude.mconcat
      [ "HttpHeaderName" Data.=: httpHeaderName,
        "Values"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> values)
      ]
