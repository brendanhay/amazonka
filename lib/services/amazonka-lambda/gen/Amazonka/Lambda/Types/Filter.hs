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
-- Module      : Amazonka.Lambda.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure within a @FilterCriteria@ object that defines an event
-- filtering pattern.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | A filter pattern. For more information on the syntax of a filter
    -- pattern, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-eventfiltering.html#filtering-syntax Filter rule syntax>.
    pattern' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pattern'', 'filter_pattern' - A filter pattern. For more information on the syntax of a filter
-- pattern, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-eventfiltering.html#filtering-syntax Filter rule syntax>.
newFilter ::
  Filter
newFilter = Filter' {pattern' = Prelude.Nothing}

-- | A filter pattern. For more information on the syntax of a filter
-- pattern, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-eventfiltering.html#filtering-syntax Filter rule syntax>.
filter_pattern :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_pattern = Lens.lens (\Filter' {pattern'} -> pattern') (\s@Filter' {} a -> s {pattern' = a} :: Filter)

instance Data.FromJSON Filter where
  parseJSON =
    Data.withObject
      "Filter"
      (\x -> Filter' Prelude.<$> (x Data..:? "Pattern"))

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt `Prelude.hashWithSalt` pattern'

instance Prelude.NFData Filter where
  rnf Filter' {..} = Prelude.rnf pattern'

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Pattern" Data..=) Prelude.<$> pattern']
      )
