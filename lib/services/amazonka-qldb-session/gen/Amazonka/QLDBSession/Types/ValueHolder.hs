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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.ValueHolder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that can contain a value in multiple encoding formats.
--
-- /See:/ 'newValueHolder' smart constructor.
data ValueHolder = ValueHolder'
  { -- | An Amazon Ion binary value contained in a @ValueHolder@ structure.
    ionBinary :: Prelude.Maybe Data.Base64,
    -- | An Amazon Ion plaintext value contained in a @ValueHolder@ structure.
    ionText :: Prelude.Maybe Prelude.Text
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
-- 'ionBinary', 'valueHolder_ionBinary' - An Amazon Ion binary value contained in a @ValueHolder@ structure.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'ionText', 'valueHolder_ionText' - An Amazon Ion plaintext value contained in a @ValueHolder@ structure.
newValueHolder ::
  ValueHolder
newValueHolder =
  ValueHolder'
    { ionBinary = Prelude.Nothing,
      ionText = Prelude.Nothing
    }

-- | An Amazon Ion binary value contained in a @ValueHolder@ structure.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
valueHolder_ionBinary :: Lens.Lens' ValueHolder (Prelude.Maybe Prelude.ByteString)
valueHolder_ionBinary = Lens.lens (\ValueHolder' {ionBinary} -> ionBinary) (\s@ValueHolder' {} a -> s {ionBinary = a} :: ValueHolder) Prelude.. Lens.mapping Data._Base64

-- | An Amazon Ion plaintext value contained in a @ValueHolder@ structure.
valueHolder_ionText :: Lens.Lens' ValueHolder (Prelude.Maybe Prelude.Text)
valueHolder_ionText = Lens.lens (\ValueHolder' {ionText} -> ionText) (\s@ValueHolder' {} a -> s {ionText = a} :: ValueHolder)

instance Data.FromJSON ValueHolder where
  parseJSON =
    Data.withObject
      "ValueHolder"
      ( \x ->
          ValueHolder'
            Prelude.<$> (x Data..:? "IonBinary")
            Prelude.<*> (x Data..:? "IonText")
      )

instance Prelude.Hashable ValueHolder where
  hashWithSalt _salt ValueHolder' {..} =
    _salt
      `Prelude.hashWithSalt` ionBinary
      `Prelude.hashWithSalt` ionText

instance Prelude.NFData ValueHolder where
  rnf ValueHolder' {..} =
    Prelude.rnf ionBinary `Prelude.seq`
      Prelude.rnf ionText

instance Data.ToJSON ValueHolder where
  toJSON ValueHolder' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IonBinary" Data..=) Prelude.<$> ionBinary,
            ("IonText" Data..=) Prelude.<$> ionText
          ]
      )
