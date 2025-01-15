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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.Taint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.TaintEffect
import qualified Amazonka.Prelude as Prelude

-- | A property that allows a node to repel a set of pods. For more
-- information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/node-taints-managed-node-groups.html Node taints on managed node groups>.
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

instance Data.FromJSON Taint where
  parseJSON =
    Data.withObject
      "Taint"
      ( \x ->
          Taint'
            Prelude.<$> (x Data..:? "effect")
            Prelude.<*> (x Data..:? "key")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable Taint where
  hashWithSalt _salt Taint' {..} =
    _salt
      `Prelude.hashWithSalt` effect
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData Taint where
  rnf Taint' {..} =
    Prelude.rnf effect `Prelude.seq`
      Prelude.rnf key `Prelude.seq`
        Prelude.rnf value

instance Data.ToJSON Taint where
  toJSON Taint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("effect" Data..=) Prelude.<$> effect,
            ("key" Data..=) Prelude.<$> key,
            ("value" Data..=) Prelude.<$> value
          ]
      )
