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
-- Module      : Network.AWS.IoTAnalytics.Types.TriggeringDataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.TriggeringDataset where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the dataset whose content generation triggers the new
-- dataset content generation.
--
-- /See:/ 'newTriggeringDataset' smart constructor.
data TriggeringDataset = TriggeringDataset'
  { -- | The name of the dataset whose content generation triggers the new
    -- dataset content generation.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON TriggeringDataset where
  parseJSON =
    Prelude.withObject
      "TriggeringDataset"
      ( \x ->
          TriggeringDataset' Prelude.<$> (x Prelude..: "name")
      )

instance Prelude.Hashable TriggeringDataset

instance Prelude.NFData TriggeringDataset

instance Prelude.ToJSON TriggeringDataset where
  toJSON TriggeringDataset' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Prelude..= name)]
      )
