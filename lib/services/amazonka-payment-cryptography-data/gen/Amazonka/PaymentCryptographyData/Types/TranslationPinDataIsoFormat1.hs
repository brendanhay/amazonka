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
-- Module      : Amazonka.PaymentCryptographyData.Types.TranslationPinDataIsoFormat1
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.TranslationPinDataIsoFormat1 where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters that are required for ISO9564 PIN format 1 tranlation.
--
-- /See:/ 'newTranslationPinDataIsoFormat1' smart constructor.
data TranslationPinDataIsoFormat1 = TranslationPinDataIsoFormat1'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TranslationPinDataIsoFormat1' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTranslationPinDataIsoFormat1 ::
  TranslationPinDataIsoFormat1
newTranslationPinDataIsoFormat1 =
  TranslationPinDataIsoFormat1'

instance
  Prelude.Hashable
    TranslationPinDataIsoFormat1
  where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData TranslationPinDataIsoFormat1 where
  rnf _ = ()

instance Data.ToJSON TranslationPinDataIsoFormat1 where
  toJSON = Prelude.const (Data.Object Prelude.mempty)
