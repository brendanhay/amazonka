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
-- Module      : Network.AWS.EKS.Types.Taint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.Taint where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.TaintEffect
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A property that allows a node to repel a set of pods.
--
-- /See:/ 'newTaint' smart constructor.
data Taint = Taint'
  { -- | The effect of the taint.
    effect :: Prelude.Maybe TaintEffect,
    -- | The key of the taint.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value of the taint.
    value :: Prelude.Maybe Prelude.Text
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
-- 'key', 'taint_key' - The key of the taint.
--
-- 'value', 'taint_value' - The value of the taint.
newTaint ::
  Taint
newTaint =
  Taint'
    { effect = Prelude.Nothing,
      key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The effect of the taint.
taint_effect :: Lens.Lens' Taint (Prelude.Maybe TaintEffect)
taint_effect = Lens.lens (\Taint' {effect} -> effect) (\s@Taint' {} a -> s {effect = a} :: Taint)

-- | The key of the taint.
taint_key :: Lens.Lens' Taint (Prelude.Maybe Prelude.Text)
taint_key = Lens.lens (\Taint' {key} -> key) (\s@Taint' {} a -> s {key = a} :: Taint)

-- | The value of the taint.
taint_value :: Lens.Lens' Taint (Prelude.Maybe Prelude.Text)
taint_value = Lens.lens (\Taint' {value} -> value) (\s@Taint' {} a -> s {value = a} :: Taint)

instance Core.FromJSON Taint where
  parseJSON =
    Core.withObject
      "Taint"
      ( \x ->
          Taint'
            Prelude.<$> (x Core..:? "effect")
            Prelude.<*> (x Core..:? "key")
            Prelude.<*> (x Core..:? "value")
      )

instance Prelude.Hashable Taint

instance Prelude.NFData Taint

instance Core.ToJSON Taint where
  toJSON Taint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("effect" Core..=) Prelude.<$> effect,
            ("key" Core..=) Prelude.<$> key,
            ("value" Core..=) Prelude.<$> value
          ]
      )
