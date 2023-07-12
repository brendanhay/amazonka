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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.OptedOutFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.OptedOutFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.OptedOutFilterName
import qualified Amazonka.Prelude as Prelude

-- | The information for opted out numbers that meet a specified criteria.
--
-- /See:/ 'newOptedOutFilter' smart constructor.
data OptedOutFilter = OptedOutFilter'
  { -- | The name of the attribute to filter on.
    name :: OptedOutFilterName,
    -- | An array of values to filter for.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptedOutFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'optedOutFilter_name' - The name of the attribute to filter on.
--
-- 'values', 'optedOutFilter_values' - An array of values to filter for.
newOptedOutFilter ::
  -- | 'name'
  OptedOutFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  OptedOutFilter
newOptedOutFilter pName_ pValues_ =
  OptedOutFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the attribute to filter on.
optedOutFilter_name :: Lens.Lens' OptedOutFilter OptedOutFilterName
optedOutFilter_name = Lens.lens (\OptedOutFilter' {name} -> name) (\s@OptedOutFilter' {} a -> s {name = a} :: OptedOutFilter)

-- | An array of values to filter for.
optedOutFilter_values :: Lens.Lens' OptedOutFilter (Prelude.NonEmpty Prelude.Text)
optedOutFilter_values = Lens.lens (\OptedOutFilter' {values} -> values) (\s@OptedOutFilter' {} a -> s {values = a} :: OptedOutFilter) Prelude.. Lens.coerced

instance Prelude.Hashable OptedOutFilter where
  hashWithSalt _salt OptedOutFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData OptedOutFilter where
  rnf OptedOutFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON OptedOutFilter where
  toJSON OptedOutFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
