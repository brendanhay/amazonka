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
-- Module      : Amazonka.WAFV2.Types.LoggingFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.LoggingFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.Filter
import Amazonka.WAFV2.Types.FilterBehavior

-- | Filtering that specifies which web requests are kept in the logs and
-- which are dropped, defined for a web ACL\'s LoggingConfiguration.
--
-- You can filter on the rule action and on the web request labels that
-- were applied by matching rules during web ACL evaluation.
--
-- /See:/ 'newLoggingFilter' smart constructor.
data LoggingFilter = LoggingFilter'
  { -- | The filters that you want to apply to the logs.
    filters :: Prelude.NonEmpty Filter,
    -- | Default handling for logs that don\'t match any of the specified
    -- filtering conditions.
    defaultBehavior :: FilterBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'loggingFilter_filters' - The filters that you want to apply to the logs.
--
-- 'defaultBehavior', 'loggingFilter_defaultBehavior' - Default handling for logs that don\'t match any of the specified
-- filtering conditions.
newLoggingFilter ::
  -- | 'filters'
  Prelude.NonEmpty Filter ->
  -- | 'defaultBehavior'
  FilterBehavior ->
  LoggingFilter
newLoggingFilter pFilters_ pDefaultBehavior_ =
  LoggingFilter'
    { filters =
        Lens.coerced Lens.# pFilters_,
      defaultBehavior = pDefaultBehavior_
    }

-- | The filters that you want to apply to the logs.
loggingFilter_filters :: Lens.Lens' LoggingFilter (Prelude.NonEmpty Filter)
loggingFilter_filters = Lens.lens (\LoggingFilter' {filters} -> filters) (\s@LoggingFilter' {} a -> s {filters = a} :: LoggingFilter) Prelude.. Lens.coerced

-- | Default handling for logs that don\'t match any of the specified
-- filtering conditions.
loggingFilter_defaultBehavior :: Lens.Lens' LoggingFilter FilterBehavior
loggingFilter_defaultBehavior = Lens.lens (\LoggingFilter' {defaultBehavior} -> defaultBehavior) (\s@LoggingFilter' {} a -> s {defaultBehavior = a} :: LoggingFilter)

instance Data.FromJSON LoggingFilter where
  parseJSON =
    Data.withObject
      "LoggingFilter"
      ( \x ->
          LoggingFilter'
            Prelude.<$> (x Data..: "Filters")
            Prelude.<*> (x Data..: "DefaultBehavior")
      )

instance Prelude.Hashable LoggingFilter where
  hashWithSalt _salt LoggingFilter' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` defaultBehavior

instance Prelude.NFData LoggingFilter where
  rnf LoggingFilter' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf defaultBehavior

instance Data.ToJSON LoggingFilter where
  toJSON LoggingFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Filters" Data..= filters),
            Prelude.Just
              ("DefaultBehavior" Data..= defaultBehavior)
          ]
      )
