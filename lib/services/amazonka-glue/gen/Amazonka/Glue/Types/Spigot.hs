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
-- Module      : Amazonka.Glue.Types.Spigot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Spigot where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a transform that writes samples of the data to an Amazon S3
-- bucket.
--
-- /See:/ 'newSpigot' smart constructor.
data Spigot = Spigot'
  { -- | The probability (a decimal value with a maximum value of 1) of picking
    -- any given record. A value of 1 indicates that each row read from the
    -- dataset should be included in the sample output.
    prob :: Prelude.Maybe Prelude.Double,
    -- | Specifies a number of records to write starting from the beginning of
    -- the dataset.
    topk :: Prelude.Maybe Prelude.Natural,
    -- | The name of the transform node.
    name :: Prelude.Text,
    -- | The data inputs identified by their node names.
    inputs :: Prelude.NonEmpty Prelude.Text,
    -- | A path in Amazon S3 where the transform will write a subset of records
    -- from the dataset to a JSON file in an Amazon S3 bucket.
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Spigot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prob', 'spigot_prob' - The probability (a decimal value with a maximum value of 1) of picking
-- any given record. A value of 1 indicates that each row read from the
-- dataset should be included in the sample output.
--
-- 'topk', 'spigot_topk' - Specifies a number of records to write starting from the beginning of
-- the dataset.
--
-- 'name', 'spigot_name' - The name of the transform node.
--
-- 'inputs', 'spigot_inputs' - The data inputs identified by their node names.
--
-- 'path', 'spigot_path' - A path in Amazon S3 where the transform will write a subset of records
-- from the dataset to a JSON file in an Amazon S3 bucket.
newSpigot ::
  -- | 'name'
  Prelude.Text ->
  -- | 'inputs'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'path'
  Prelude.Text ->
  Spigot
newSpigot pName_ pInputs_ pPath_ =
  Spigot'
    { prob = Prelude.Nothing,
      topk = Prelude.Nothing,
      name = pName_,
      inputs = Lens.coerced Lens.# pInputs_,
      path = pPath_
    }

-- | The probability (a decimal value with a maximum value of 1) of picking
-- any given record. A value of 1 indicates that each row read from the
-- dataset should be included in the sample output.
spigot_prob :: Lens.Lens' Spigot (Prelude.Maybe Prelude.Double)
spigot_prob = Lens.lens (\Spigot' {prob} -> prob) (\s@Spigot' {} a -> s {prob = a} :: Spigot)

-- | Specifies a number of records to write starting from the beginning of
-- the dataset.
spigot_topk :: Lens.Lens' Spigot (Prelude.Maybe Prelude.Natural)
spigot_topk = Lens.lens (\Spigot' {topk} -> topk) (\s@Spigot' {} a -> s {topk = a} :: Spigot)

-- | The name of the transform node.
spigot_name :: Lens.Lens' Spigot Prelude.Text
spigot_name = Lens.lens (\Spigot' {name} -> name) (\s@Spigot' {} a -> s {name = a} :: Spigot)

-- | The data inputs identified by their node names.
spigot_inputs :: Lens.Lens' Spigot (Prelude.NonEmpty Prelude.Text)
spigot_inputs = Lens.lens (\Spigot' {inputs} -> inputs) (\s@Spigot' {} a -> s {inputs = a} :: Spigot) Prelude.. Lens.coerced

-- | A path in Amazon S3 where the transform will write a subset of records
-- from the dataset to a JSON file in an Amazon S3 bucket.
spigot_path :: Lens.Lens' Spigot Prelude.Text
spigot_path = Lens.lens (\Spigot' {path} -> path) (\s@Spigot' {} a -> s {path = a} :: Spigot)

instance Core.FromJSON Spigot where
  parseJSON =
    Core.withObject
      "Spigot"
      ( \x ->
          Spigot'
            Prelude.<$> (x Core..:? "Prob")
            Prelude.<*> (x Core..:? "Topk")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Inputs")
            Prelude.<*> (x Core..: "Path")
      )

instance Prelude.Hashable Spigot where
  hashWithSalt _salt Spigot' {..} =
    _salt `Prelude.hashWithSalt` prob
      `Prelude.hashWithSalt` topk
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` path

instance Prelude.NFData Spigot where
  rnf Spigot' {..} =
    Prelude.rnf prob
      `Prelude.seq` Prelude.rnf topk
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf path

instance Core.ToJSON Spigot where
  toJSON Spigot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Prob" Core..=) Prelude.<$> prob,
            ("Topk" Core..=) Prelude.<$> topk,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("Inputs" Core..= inputs),
            Prelude.Just ("Path" Core..= path)
          ]
      )
