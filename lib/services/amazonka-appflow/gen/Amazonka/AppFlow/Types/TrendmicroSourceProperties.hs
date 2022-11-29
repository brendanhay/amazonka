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
-- Module      : Amazonka.AppFlow.Types.TrendmicroSourceProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.TrendmicroSourceProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when using Trend Micro as a flow source.
--
-- /See:/ 'newTrendmicroSourceProperties' smart constructor.
data TrendmicroSourceProperties = TrendmicroSourceProperties'
  { -- | The object specified in the Trend Micro flow source.
    object' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrendmicroSourceProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'object'', 'trendmicroSourceProperties_object' - The object specified in the Trend Micro flow source.
newTrendmicroSourceProperties ::
  -- | 'object''
  Prelude.Text ->
  TrendmicroSourceProperties
newTrendmicroSourceProperties pObject_ =
  TrendmicroSourceProperties' {object' = pObject_}

-- | The object specified in the Trend Micro flow source.
trendmicroSourceProperties_object :: Lens.Lens' TrendmicroSourceProperties Prelude.Text
trendmicroSourceProperties_object = Lens.lens (\TrendmicroSourceProperties' {object'} -> object') (\s@TrendmicroSourceProperties' {} a -> s {object' = a} :: TrendmicroSourceProperties)

instance Core.FromJSON TrendmicroSourceProperties where
  parseJSON =
    Core.withObject
      "TrendmicroSourceProperties"
      ( \x ->
          TrendmicroSourceProperties'
            Prelude.<$> (x Core..: "object")
      )

instance Prelude.Hashable TrendmicroSourceProperties where
  hashWithSalt _salt TrendmicroSourceProperties' {..} =
    _salt `Prelude.hashWithSalt` object'

instance Prelude.NFData TrendmicroSourceProperties where
  rnf TrendmicroSourceProperties' {..} =
    Prelude.rnf object'

instance Core.ToJSON TrendmicroSourceProperties where
  toJSON TrendmicroSourceProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("object" Core..= object')]
      )
