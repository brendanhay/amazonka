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
-- Module      : Network.AWS.ELBv2.Types.QueryStringConditionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.QueryStringConditionConfig where

import Network.AWS.ELBv2.Types.QueryStringKeyValuePair
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a query string condition.
--
-- The query string component of a URI starts after the first \'?\'
-- character and is terminated by either a \'#\' character or the end of
-- the URI. A typical query string contains key\/value pairs separated by
-- \'&\' characters. The allowed characters are specified by RFC 3986. Any
-- character can be percentage encoded.
--
-- /See:/ 'newQueryStringConditionConfig' smart constructor.
data QueryStringConditionConfig = QueryStringConditionConfig'
  { -- | One or more key\/value pairs or values to find in the query string. The
    -- maximum size of each string is 128 characters. The comparison is case
    -- insensitive. The following wildcard characters are supported: * (matches
    -- 0 or more characters) and ? (matches exactly 1 character). To search for
    -- a literal \'*\' or \'?\' character in a query string, you must escape
    -- these characters in @Values@ using a \'\\\' character.
    --
    -- If you specify multiple key\/value pairs or values, the condition is
    -- satisfied if one of them is found in the query string.
    values :: Prelude.Maybe [QueryStringKeyValuePair]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'QueryStringConditionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'queryStringConditionConfig_values' - One or more key\/value pairs or values to find in the query string. The
-- maximum size of each string is 128 characters. The comparison is case
-- insensitive. The following wildcard characters are supported: * (matches
-- 0 or more characters) and ? (matches exactly 1 character). To search for
-- a literal \'*\' or \'?\' character in a query string, you must escape
-- these characters in @Values@ using a \'\\\' character.
--
-- If you specify multiple key\/value pairs or values, the condition is
-- satisfied if one of them is found in the query string.
newQueryStringConditionConfig ::
  QueryStringConditionConfig
newQueryStringConditionConfig =
  QueryStringConditionConfig'
    { values =
        Prelude.Nothing
    }

-- | One or more key\/value pairs or values to find in the query string. The
-- maximum size of each string is 128 characters. The comparison is case
-- insensitive. The following wildcard characters are supported: * (matches
-- 0 or more characters) and ? (matches exactly 1 character). To search for
-- a literal \'*\' or \'?\' character in a query string, you must escape
-- these characters in @Values@ using a \'\\\' character.
--
-- If you specify multiple key\/value pairs or values, the condition is
-- satisfied if one of them is found in the query string.
queryStringConditionConfig_values :: Lens.Lens' QueryStringConditionConfig (Prelude.Maybe [QueryStringKeyValuePair])
queryStringConditionConfig_values = Lens.lens (\QueryStringConditionConfig' {values} -> values) (\s@QueryStringConditionConfig' {} a -> s {values = a} :: QueryStringConditionConfig) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromXML QueryStringConditionConfig where
  parseXML x =
    QueryStringConditionConfig'
      Prelude.<$> ( x Prelude..@? "Values" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )

instance Prelude.Hashable QueryStringConditionConfig

instance Prelude.NFData QueryStringConditionConfig

instance Prelude.ToQuery QueryStringConditionConfig where
  toQuery QueryStringConditionConfig' {..} =
    Prelude.mconcat
      [ "Values"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> values)
      ]
