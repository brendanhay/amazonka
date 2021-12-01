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
-- Module      : Amazonka.QLDBSession.Types.ValueHolder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.ValueHolder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that can contain a value in multiple encoding formats.
--
-- /See:/ 'newValueHolder' smart constructor.
data ValueHolder = ValueHolder'
  { -- | An Amazon Ion plaintext value contained in a @ValueHolder@ structure.
    ionText :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Ion binary value contained in a @ValueHolder@ structure.
    ionBinary :: Prelude.Maybe Core.Base64
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValueHolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ionText', 'valueHolder_ionText' - An Amazon Ion plaintext value contained in a @ValueHolder@ structure.
--
-- 'ionBinary', 'valueHolder_ionBinary' - An Amazon Ion binary value contained in a @ValueHolder@ structure.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
newValueHolder ::
  ValueHolder
newValueHolder =
  ValueHolder'
    { ionText = Prelude.Nothing,
      ionBinary = Prelude.Nothing
    }

-- | An Amazon Ion plaintext value contained in a @ValueHolder@ structure.
valueHolder_ionText :: Lens.Lens' ValueHolder (Prelude.Maybe Prelude.Text)
valueHolder_ionText = Lens.lens (\ValueHolder' {ionText} -> ionText) (\s@ValueHolder' {} a -> s {ionText = a} :: ValueHolder)

-- | An Amazon Ion binary value contained in a @ValueHolder@ structure.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
valueHolder_ionBinary :: Lens.Lens' ValueHolder (Prelude.Maybe Prelude.ByteString)
valueHolder_ionBinary = Lens.lens (\ValueHolder' {ionBinary} -> ionBinary) (\s@ValueHolder' {} a -> s {ionBinary = a} :: ValueHolder) Prelude.. Lens.mapping Core._Base64

instance Core.FromJSON ValueHolder where
  parseJSON =
    Core.withObject
      "ValueHolder"
      ( \x ->
          ValueHolder'
            Prelude.<$> (x Core..:? "IonText")
            Prelude.<*> (x Core..:? "IonBinary")
      )

instance Prelude.Hashable ValueHolder where
  hashWithSalt salt' ValueHolder' {..} =
    salt' `Prelude.hashWithSalt` ionBinary
      `Prelude.hashWithSalt` ionText

instance Prelude.NFData ValueHolder where
  rnf ValueHolder' {..} =
    Prelude.rnf ionText
      `Prelude.seq` Prelude.rnf ionBinary

instance Core.ToJSON ValueHolder where
  toJSON ValueHolder' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("IonText" Core..=) Prelude.<$> ionText,
            ("IonBinary" Core..=) Prelude.<$> ionBinary
          ]
      )
