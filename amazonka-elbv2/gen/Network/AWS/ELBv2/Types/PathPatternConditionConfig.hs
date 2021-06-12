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
-- Module      : Network.AWS.ELBv2.Types.PathPatternConditionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.PathPatternConditionConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a path pattern condition.
--
-- /See:/ 'newPathPatternConditionConfig' smart constructor.
data PathPatternConditionConfig = PathPatternConditionConfig'
  { -- | One or more path patterns to compare against the request URL. The
    -- maximum size of each string is 128 characters. The comparison is case
    -- sensitive. The following wildcard characters are supported: * (matches 0
    -- or more characters) and ? (matches exactly 1 character).
    --
    -- If you specify multiple strings, the condition is satisfied if one of
    -- them matches the request URL. The path pattern is compared only to the
    -- path of the URL, not to its query string. To compare against the query
    -- string, use QueryStringConditionConfig.
    values :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PathPatternConditionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'pathPatternConditionConfig_values' - One or more path patterns to compare against the request URL. The
-- maximum size of each string is 128 characters. The comparison is case
-- sensitive. The following wildcard characters are supported: * (matches 0
-- or more characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of
-- them matches the request URL. The path pattern is compared only to the
-- path of the URL, not to its query string. To compare against the query
-- string, use QueryStringConditionConfig.
newPathPatternConditionConfig ::
  PathPatternConditionConfig
newPathPatternConditionConfig =
  PathPatternConditionConfig' {values = Core.Nothing}

-- | One or more path patterns to compare against the request URL. The
-- maximum size of each string is 128 characters. The comparison is case
-- sensitive. The following wildcard characters are supported: * (matches 0
-- or more characters) and ? (matches exactly 1 character).
--
-- If you specify multiple strings, the condition is satisfied if one of
-- them matches the request URL. The path pattern is compared only to the
-- path of the URL, not to its query string. To compare against the query
-- string, use QueryStringConditionConfig.
pathPatternConditionConfig_values :: Lens.Lens' PathPatternConditionConfig (Core.Maybe [Core.Text])
pathPatternConditionConfig_values = Lens.lens (\PathPatternConditionConfig' {values} -> values) (\s@PathPatternConditionConfig' {} a -> s {values = a} :: PathPatternConditionConfig) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML PathPatternConditionConfig where
  parseXML x =
    PathPatternConditionConfig'
      Core.<$> ( x Core..@? "Values" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )

instance Core.Hashable PathPatternConditionConfig

instance Core.NFData PathPatternConditionConfig

instance Core.ToQuery PathPatternConditionConfig where
  toQuery PathPatternConditionConfig' {..} =
    Core.mconcat
      [ "Values"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> values)
      ]
