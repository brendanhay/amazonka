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
-- Module      : Network.AWS.QLDB.Types.ValueHolder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDB.Types.ValueHolder where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure that can contain a value in multiple encoding formats.
--
-- /See:/ 'newValueHolder' smart constructor.
data ValueHolder = ValueHolder'
  { -- | An Amazon Ion plaintext value contained in a @ValueHolder@ structure.
    ionText :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValueHolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ionText', 'valueHolder_ionText' - An Amazon Ion plaintext value contained in a @ValueHolder@ structure.
newValueHolder ::
  ValueHolder
newValueHolder =
  ValueHolder' {ionText = Prelude.Nothing}

-- | An Amazon Ion plaintext value contained in a @ValueHolder@ structure.
valueHolder_ionText :: Lens.Lens' ValueHolder (Prelude.Maybe Prelude.Text)
valueHolder_ionText = Lens.lens (\ValueHolder' {ionText} -> ionText) (\s@ValueHolder' {} a -> s {ionText = a} :: ValueHolder) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON ValueHolder where
  parseJSON =
    Core.withObject
      "ValueHolder"
      ( \x ->
          ValueHolder' Prelude.<$> (x Core..:? "IonText")
      )

instance Prelude.Hashable ValueHolder

instance Prelude.NFData ValueHolder

instance Core.ToJSON ValueHolder where
  toJSON ValueHolder' {..} =
    Core.object
      ( Prelude.catMaybes
          [("IonText" Core..=) Prelude.<$> ionText]
      )
