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
-- Module      : Amazonka.LookoutMetrics.Types.AttributeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An attribute value.
--
-- /See:/ 'newAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | A binary value.
    b :: Prelude.Maybe Prelude.Text,
    -- | A list of binary values.
    bs :: Prelude.Maybe [Prelude.Text],
    -- | A number.
    n :: Prelude.Maybe Prelude.Text,
    -- | A list of numbers.
    ns :: Prelude.Maybe [Prelude.Text],
    -- | A string.
    s :: Prelude.Maybe Prelude.Text,
    -- | A list of strings.
    ss :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'b', 'attributeValue_b' - A binary value.
--
-- 'bs', 'attributeValue_bs' - A list of binary values.
--
-- 'n', 'attributeValue_n' - A number.
--
-- 'ns', 'attributeValue_ns' - A list of numbers.
--
-- 's', 'attributeValue_s' - A string.
--
-- 'ss', 'attributeValue_ss' - A list of strings.
newAttributeValue ::
  AttributeValue
newAttributeValue =
  AttributeValue'
    { b = Prelude.Nothing,
      bs = Prelude.Nothing,
      n = Prelude.Nothing,
      ns = Prelude.Nothing,
      s = Prelude.Nothing,
      ss = Prelude.Nothing
    }

-- | A binary value.
attributeValue_b :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Text)
attributeValue_b = Lens.lens (\AttributeValue' {b} -> b) (\s@AttributeValue' {} a -> s {b = a} :: AttributeValue)

-- | A list of binary values.
attributeValue_bs :: Lens.Lens' AttributeValue (Prelude.Maybe [Prelude.Text])
attributeValue_bs = Lens.lens (\AttributeValue' {bs} -> bs) (\s@AttributeValue' {} a -> s {bs = a} :: AttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | A number.
attributeValue_n :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Text)
attributeValue_n = Lens.lens (\AttributeValue' {n} -> n) (\s@AttributeValue' {} a -> s {n = a} :: AttributeValue)

-- | A list of numbers.
attributeValue_ns :: Lens.Lens' AttributeValue (Prelude.Maybe [Prelude.Text])
attributeValue_ns = Lens.lens (\AttributeValue' {ns} -> ns) (\s@AttributeValue' {} a -> s {ns = a} :: AttributeValue) Prelude.. Lens.mapping Lens.coerced

-- | A string.
attributeValue_s :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Text)
attributeValue_s = Lens.lens (\AttributeValue' {s} -> s) (\s@AttributeValue' {} a -> s {s = a} :: AttributeValue)

-- | A list of strings.
attributeValue_ss :: Lens.Lens' AttributeValue (Prelude.Maybe [Prelude.Text])
attributeValue_ss = Lens.lens (\AttributeValue' {ss} -> ss) (\s@AttributeValue' {} a -> s {ss = a} :: AttributeValue) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AttributeValue where
  parseJSON =
    Data.withObject
      "AttributeValue"
      ( \x ->
          AttributeValue'
            Prelude.<$> (x Data..:? "B")
            Prelude.<*> (x Data..:? "BS" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "N")
            Prelude.<*> (x Data..:? "NS" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "S")
            Prelude.<*> (x Data..:? "SS" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AttributeValue where
  hashWithSalt _salt AttributeValue' {..} =
    _salt
      `Prelude.hashWithSalt` b
      `Prelude.hashWithSalt` bs
      `Prelude.hashWithSalt` n
      `Prelude.hashWithSalt` ns
      `Prelude.hashWithSalt` s
      `Prelude.hashWithSalt` ss

instance Prelude.NFData AttributeValue where
  rnf AttributeValue' {..} =
    Prelude.rnf b
      `Prelude.seq` Prelude.rnf bs
      `Prelude.seq` Prelude.rnf n
      `Prelude.seq` Prelude.rnf ns
      `Prelude.seq` Prelude.rnf s
      `Prelude.seq` Prelude.rnf ss
