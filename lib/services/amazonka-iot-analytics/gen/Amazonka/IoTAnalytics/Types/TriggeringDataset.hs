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
-- Module      : Amazonka.IoTAnalytics.Types.TriggeringDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.TriggeringDataset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the dataset whose content generation triggers the new
-- dataset content generation.
--
-- /See:/ 'newTriggeringDataset' smart constructor.
data TriggeringDataset = TriggeringDataset'
  { -- | The name of the dataset whose content generation triggers the new
    -- dataset content generation.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TriggeringDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'triggeringDataset_name' - The name of the dataset whose content generation triggers the new
-- dataset content generation.
newTriggeringDataset ::
  -- | 'name'
  Prelude.Text ->
  TriggeringDataset
newTriggeringDataset pName_ =
  TriggeringDataset' {name = pName_}

-- | The name of the dataset whose content generation triggers the new
-- dataset content generation.
triggeringDataset_name :: Lens.Lens' TriggeringDataset Prelude.Text
triggeringDataset_name = Lens.lens (\TriggeringDataset' {name} -> name) (\s@TriggeringDataset' {} a -> s {name = a} :: TriggeringDataset)

instance Data.FromJSON TriggeringDataset where
  parseJSON =
    Data.withObject
      "TriggeringDataset"
      ( \x ->
          TriggeringDataset' Prelude.<$> (x Data..: "name")
      )

instance Prelude.Hashable TriggeringDataset where
  hashWithSalt _salt TriggeringDataset' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData TriggeringDataset where
  rnf TriggeringDataset' {..} = Prelude.rnf name

instance Data.ToJSON TriggeringDataset where
  toJSON TriggeringDataset' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )
