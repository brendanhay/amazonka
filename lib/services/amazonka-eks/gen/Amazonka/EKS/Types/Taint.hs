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
-- Module      : Amazonka.EKS.Types.Taint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.Taint where

import qualified Amazonka.Core as Core
import Amazonka.EKS.Types.TaintEffect
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A property that allows a node to repel a set of pods.
--
-- /See:/ 'newTaint' smart constructor.
data Taint = Taint'
  { -- | The effect of the taint.
    effect :: Prelude.Maybe TaintEffect,
    -- | The value of the taint.
    value :: Prelude.Maybe Prelude.Text,
    -- | The key of the taint.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Taint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effect', 'taint_effect' - The effect of the taint.
--
-- 'value', 'taint_value' - The value of the taint.
--
-- 'key', 'taint_key' - The key of the taint.
newTaint ::
  Taint
newTaint =
  Taint'
    { effect = Prelude.Nothing,
      value = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The effect of the taint.
taint_effect :: Lens.Lens' Taint (Prelude.Maybe TaintEffect)
taint_effect = Lens.lens (\Taint' {effect} -> effect) (\s@Taint' {} a -> s {effect = a} :: Taint)

-- | The value of the taint.
taint_value :: Lens.Lens' Taint (Prelude.Maybe Prelude.Text)
taint_value = Lens.lens (\Taint' {value} -> value) (\s@Taint' {} a -> s {value = a} :: Taint)

-- | The key of the taint.
taint_key :: Lens.Lens' Taint (Prelude.Maybe Prelude.Text)
taint_key = Lens.lens (\Taint' {key} -> key) (\s@Taint' {} a -> s {key = a} :: Taint)

instance Core.FromJSON Taint where
  parseJSON =
    Core.withObject
      "Taint"
      ( \x ->
          Taint'
            Prelude.<$> (x Core..:? "effect")
            Prelude.<*> (x Core..:? "value")
            Prelude.<*> (x Core..:? "key")
      )

instance Prelude.Hashable Taint where
  hashWithSalt salt' Taint' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` effect

instance Prelude.NFData Taint where
  rnf Taint' {..} =
    Prelude.rnf effect `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf value

instance Core.ToJSON Taint where
  toJSON Taint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("effect" Core..=) Prelude.<$> effect,
            ("value" Core..=) Prelude.<$> value,
            ("key" Core..=) Prelude.<$> key
          ]
      )
