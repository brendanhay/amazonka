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
-- Module      : Amazonka.PaymentCryptographyData.Types.TranslationIsoFormats
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.TranslationIsoFormats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.TranslationPinDataIsoFormat034
import Amazonka.PaymentCryptographyData.Types.TranslationPinDataIsoFormat1
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required for translation between ISO9564 PIN block
-- formats 0,1,3,4.
--
-- /See:/ 'newTranslationIsoFormats' smart constructor.
data TranslationIsoFormats = TranslationIsoFormats'
  { -- | Parameters that are required for ISO9564 PIN format 0 tranlation.
    isoFormat0 :: Prelude.Maybe TranslationPinDataIsoFormat034,
    -- | Parameters that are required for ISO9564 PIN format 1 tranlation.
    isoFormat1 :: Prelude.Maybe TranslationPinDataIsoFormat1,
    -- | Parameters that are required for ISO9564 PIN format 3 tranlation.
    isoFormat3 :: Prelude.Maybe TranslationPinDataIsoFormat034,
    -- | Parameters that are required for ISO9564 PIN format 4 tranlation.
    isoFormat4 :: Prelude.Maybe TranslationPinDataIsoFormat034
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslationIsoFormats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isoFormat0', 'translationIsoFormats_isoFormat0' - Parameters that are required for ISO9564 PIN format 0 tranlation.
--
-- 'isoFormat1', 'translationIsoFormats_isoFormat1' - Parameters that are required for ISO9564 PIN format 1 tranlation.
--
-- 'isoFormat3', 'translationIsoFormats_isoFormat3' - Parameters that are required for ISO9564 PIN format 3 tranlation.
--
-- 'isoFormat4', 'translationIsoFormats_isoFormat4' - Parameters that are required for ISO9564 PIN format 4 tranlation.
newTranslationIsoFormats ::
  TranslationIsoFormats
newTranslationIsoFormats =
  TranslationIsoFormats'
    { isoFormat0 =
        Prelude.Nothing,
      isoFormat1 = Prelude.Nothing,
      isoFormat3 = Prelude.Nothing,
      isoFormat4 = Prelude.Nothing
    }

-- | Parameters that are required for ISO9564 PIN format 0 tranlation.
translationIsoFormats_isoFormat0 :: Lens.Lens' TranslationIsoFormats (Prelude.Maybe TranslationPinDataIsoFormat034)
translationIsoFormats_isoFormat0 = Lens.lens (\TranslationIsoFormats' {isoFormat0} -> isoFormat0) (\s@TranslationIsoFormats' {} a -> s {isoFormat0 = a} :: TranslationIsoFormats)

-- | Parameters that are required for ISO9564 PIN format 1 tranlation.
translationIsoFormats_isoFormat1 :: Lens.Lens' TranslationIsoFormats (Prelude.Maybe TranslationPinDataIsoFormat1)
translationIsoFormats_isoFormat1 = Lens.lens (\TranslationIsoFormats' {isoFormat1} -> isoFormat1) (\s@TranslationIsoFormats' {} a -> s {isoFormat1 = a} :: TranslationIsoFormats)

-- | Parameters that are required for ISO9564 PIN format 3 tranlation.
translationIsoFormats_isoFormat3 :: Lens.Lens' TranslationIsoFormats (Prelude.Maybe TranslationPinDataIsoFormat034)
translationIsoFormats_isoFormat3 = Lens.lens (\TranslationIsoFormats' {isoFormat3} -> isoFormat3) (\s@TranslationIsoFormats' {} a -> s {isoFormat3 = a} :: TranslationIsoFormats)

-- | Parameters that are required for ISO9564 PIN format 4 tranlation.
translationIsoFormats_isoFormat4 :: Lens.Lens' TranslationIsoFormats (Prelude.Maybe TranslationPinDataIsoFormat034)
translationIsoFormats_isoFormat4 = Lens.lens (\TranslationIsoFormats' {isoFormat4} -> isoFormat4) (\s@TranslationIsoFormats' {} a -> s {isoFormat4 = a} :: TranslationIsoFormats)

instance Prelude.Hashable TranslationIsoFormats where
  hashWithSalt _salt TranslationIsoFormats' {..} =
    _salt
      `Prelude.hashWithSalt` isoFormat0
      `Prelude.hashWithSalt` isoFormat1
      `Prelude.hashWithSalt` isoFormat3
      `Prelude.hashWithSalt` isoFormat4

instance Prelude.NFData TranslationIsoFormats where
  rnf TranslationIsoFormats' {..} =
    Prelude.rnf isoFormat0
      `Prelude.seq` Prelude.rnf isoFormat1
      `Prelude.seq` Prelude.rnf isoFormat3
      `Prelude.seq` Prelude.rnf isoFormat4

instance Data.ToJSON TranslationIsoFormats where
  toJSON TranslationIsoFormats' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IsoFormat0" Data..=) Prelude.<$> isoFormat0,
            ("IsoFormat1" Data..=) Prelude.<$> isoFormat1,
            ("IsoFormat3" Data..=) Prelude.<$> isoFormat3,
            ("IsoFormat4" Data..=) Prelude.<$> isoFormat4
          ]
      )
