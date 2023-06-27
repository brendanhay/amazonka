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
-- Module      : Amazonka.PaymentCryptographyData.Types.AsymmetricEncryptionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.AsymmetricEncryptionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PaymentCryptographyData.Types.PaddingType
import qualified Amazonka.Prelude as Prelude

-- | Parameters for plaintext encryption using asymmetric keys.
--
-- /See:/ 'newAsymmetricEncryptionAttributes' smart constructor.
data AsymmetricEncryptionAttributes = AsymmetricEncryptionAttributes'
  { -- | The padding to be included with the data.
    paddingType :: Prelude.Maybe PaddingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AsymmetricEncryptionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paddingType', 'asymmetricEncryptionAttributes_paddingType' - The padding to be included with the data.
newAsymmetricEncryptionAttributes ::
  AsymmetricEncryptionAttributes
newAsymmetricEncryptionAttributes =
  AsymmetricEncryptionAttributes'
    { paddingType =
        Prelude.Nothing
    }

-- | The padding to be included with the data.
asymmetricEncryptionAttributes_paddingType :: Lens.Lens' AsymmetricEncryptionAttributes (Prelude.Maybe PaddingType)
asymmetricEncryptionAttributes_paddingType = Lens.lens (\AsymmetricEncryptionAttributes' {paddingType} -> paddingType) (\s@AsymmetricEncryptionAttributes' {} a -> s {paddingType = a} :: AsymmetricEncryptionAttributes)

instance
  Prelude.Hashable
    AsymmetricEncryptionAttributes
  where
  hashWithSalt
    _salt
    AsymmetricEncryptionAttributes' {..} =
      _salt `Prelude.hashWithSalt` paddingType

instance
  Prelude.NFData
    AsymmetricEncryptionAttributes
  where
  rnf AsymmetricEncryptionAttributes' {..} =
    Prelude.rnf paddingType

instance Data.ToJSON AsymmetricEncryptionAttributes where
  toJSON AsymmetricEncryptionAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [("PaddingType" Data..=) Prelude.<$> paddingType]
      )
