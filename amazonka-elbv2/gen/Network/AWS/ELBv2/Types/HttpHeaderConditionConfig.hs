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
-- Module      : Network.AWS.ELBv2.Types.HttpHeaderConditionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.HttpHeaderConditionConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an HTTP header condition.
--
-- There is a set of standard HTTP header fields. You can also define
-- custom HTTP header fields.
--
-- /See:/ 'newHttpHeaderConditionConfig' smart constructor.
data HttpHeaderConditionConfig = HttpHeaderConditionConfig'
  { -- | One or more strings to compare against the value of the HTTP header. The
    -- maximum size of each string is 128 characters. The comparison strings
    -- are case insensitive. The following wildcard characters are supported: *
    -- (matches 0 or more characters) and ? (matches exactly 1 character).
    --
    -- If the same header appears multiple times in the request, we search them
    -- in order until a match is found.
    --
    -- If you specify multiple strings, the condition is satisfied if one of
    -- the strings matches the value of the HTTP header. To require that all of
    -- the strings are a match, create one condition per string.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The name of the HTTP header field. The maximum size is 40 characters.
    -- The header name is case insensitive. The allowed characters are
    -- specified by RFC 7230. Wildcards are not supported.
    --
    -- You can\'t use an HTTP header condition to specify the host header. Use
    -- HostHeaderConditionConfig to specify a host header condition.
    httpHeaderName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HttpHeaderConditionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'httpHeaderConditionConfig_values' - One or more strings to compare against the value of the HTTP header. The
-- maximum size of each string is 128 characters. The comparison strings
-- are case insensitive. The following wildcard characters are supported: *
-- (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If the same header appears multiple times in the request, we search them
-- in order until a match is found.
--
-- If you specify multiple strings, the condition is satisfied if one of
-- the strings matches the value of the HTTP header. To require that all of
-- the strings are a match, create one condition per string.
--
-- 'httpHeaderName', 'httpHeaderConditionConfig_httpHeaderName' - The name of the HTTP header field. The maximum size is 40 characters.
-- The header name is case insensitive. The allowed characters are
-- specified by RFC 7230. Wildcards are not supported.
--
-- You can\'t use an HTTP header condition to specify the host header. Use
-- HostHeaderConditionConfig to specify a host header condition.
newHttpHeaderConditionConfig ::
  HttpHeaderConditionConfig
newHttpHeaderConditionConfig =
  HttpHeaderConditionConfig'
    { values =
        Prelude.Nothing,
      httpHeaderName = Prelude.Nothing
    }

-- | One or more strings to compare against the value of the HTTP header. The
-- maximum size of each string is 128 characters. The comparison strings
-- are case insensitive. The following wildcard characters are supported: *
-- (matches 0 or more characters) and ? (matches exactly 1 character).
--
-- If the same header appears multiple times in the request, we search them
-- in order until a match is found.
--
-- If you specify multiple strings, the condition is satisfied if one of
-- the strings matches the value of the HTTP header. To require that all of
-- the strings are a match, create one condition per string.
httpHeaderConditionConfig_values :: Lens.Lens' HttpHeaderConditionConfig (Prelude.Maybe [Prelude.Text])
httpHeaderConditionConfig_values = Lens.lens (\HttpHeaderConditionConfig' {values} -> values) (\s@HttpHeaderConditionConfig' {} a -> s {values = a} :: HttpHeaderConditionConfig) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the HTTP header field. The maximum size is 40 characters.
-- The header name is case insensitive. The allowed characters are
-- specified by RFC 7230. Wildcards are not supported.
--
-- You can\'t use an HTTP header condition to specify the host header. Use
-- HostHeaderConditionConfig to specify a host header condition.
httpHeaderConditionConfig_httpHeaderName :: Lens.Lens' HttpHeaderConditionConfig (Prelude.Maybe Prelude.Text)
httpHeaderConditionConfig_httpHeaderName = Lens.lens (\HttpHeaderConditionConfig' {httpHeaderName} -> httpHeaderName) (\s@HttpHeaderConditionConfig' {} a -> s {httpHeaderName = a} :: HttpHeaderConditionConfig)

instance Prelude.FromXML HttpHeaderConditionConfig where
  parseXML x =
    HttpHeaderConditionConfig'
      Prelude.<$> ( x Prelude..@? "Values" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "HttpHeaderName")

instance Prelude.Hashable HttpHeaderConditionConfig

instance Prelude.NFData HttpHeaderConditionConfig

instance Prelude.ToQuery HttpHeaderConditionConfig where
  toQuery HttpHeaderConditionConfig' {..} =
    Prelude.mconcat
      [ "Values"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> values),
        "HttpHeaderName" Prelude.=: httpHeaderName
      ]
