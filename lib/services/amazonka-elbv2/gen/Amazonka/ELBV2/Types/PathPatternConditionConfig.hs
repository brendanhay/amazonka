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
-- Module      : Amazonka.ELBV2.Types.PathPatternConditionConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.PathPatternConditionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a path pattern condition.
--
-- /See:/ 'newPathPatternConditionConfig' smart constructor.
data PathPatternConditionConfig = PathPatternConditionConfig'
  { -- | The path patterns to compare against the request URL. The maximum size
    -- of each string is 128 characters. The comparison is case sensitive. The
    -- following wildcard characters are supported: * (matches 0 or more
    -- characters) and ? (matches exactly 1 character).
    --
    -- If you specify multiple strings, the condition is satisfied if one of
    -- them matches the request URL. The path pattern is compared only to the
    -- path of the URL, not to its query string. To compare against the query
    -- string, use QueryStringConditionConfig.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PathPatternConditionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'pathPatternConditionConfig_values' - The path patterns to compare against the request URL. The maximum size
-- of each string is 128 characters. The comparison is case sensitive. The
-- following wildcard characters are supported: * (matches 0 or more
-- characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of
-- them matches the request URL. The path pattern is compared only to the
-- path of the URL, not to its query string. To compare against the query
-- string, use QueryStringConditionConfig.
newPathPatternConditionConfig ::
  PathPatternConditionConfig
newPathPatternConditionConfig =
  PathPatternConditionConfig'
    { values =
        Prelude.Nothing
    }

-- | The path patterns to compare against the request URL. The maximum size
-- of each string is 128 characters. The comparison is case sensitive. The
-- following wildcard characters are supported: * (matches 0 or more
-- characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of
-- them matches the request URL. The path pattern is compared only to the
-- path of the URL, not to its query string. To compare against the query
-- string, use QueryStringConditionConfig.
pathPatternConditionConfig_values :: Lens.Lens' PathPatternConditionConfig (Prelude.Maybe [Prelude.Text])
pathPatternConditionConfig_values = Lens.lens (\PathPatternConditionConfig' {values} -> values) (\s@PathPatternConditionConfig' {} a -> s {values = a} :: PathPatternConditionConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML PathPatternConditionConfig where
  parseXML x =
    PathPatternConditionConfig'
      Prelude.<$> ( x Data..@? "Values" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable PathPatternConditionConfig where
  hashWithSalt _salt PathPatternConditionConfig' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData PathPatternConditionConfig where
  rnf PathPatternConditionConfig' {..} =
    Prelude.rnf values

instance Data.ToQuery PathPatternConditionConfig where
  toQuery PathPatternConditionConfig' {..} =
    Prelude.mconcat
      [ "Values"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> values)
      ]
