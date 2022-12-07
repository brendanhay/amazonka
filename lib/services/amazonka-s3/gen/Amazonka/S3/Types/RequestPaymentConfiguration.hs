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
-- Module      : Amazonka.S3.Types.RequestPaymentConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.RequestPaymentConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.Payer

-- | Container for Payer.
--
-- /See:/ 'newRequestPaymentConfiguration' smart constructor.
data RequestPaymentConfiguration = RequestPaymentConfiguration'
  { -- | Specifies who pays for the download and request fees.
    payer :: Payer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestPaymentConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payer', 'requestPaymentConfiguration_payer' - Specifies who pays for the download and request fees.
newRequestPaymentConfiguration ::
  -- | 'payer'
  Payer ->
  RequestPaymentConfiguration
newRequestPaymentConfiguration pPayer_ =
  RequestPaymentConfiguration' {payer = pPayer_}

-- | Specifies who pays for the download and request fees.
requestPaymentConfiguration_payer :: Lens.Lens' RequestPaymentConfiguration Payer
requestPaymentConfiguration_payer = Lens.lens (\RequestPaymentConfiguration' {payer} -> payer) (\s@RequestPaymentConfiguration' {} a -> s {payer = a} :: RequestPaymentConfiguration)

instance Prelude.Hashable RequestPaymentConfiguration where
  hashWithSalt _salt RequestPaymentConfiguration' {..} =
    _salt `Prelude.hashWithSalt` payer

instance Prelude.NFData RequestPaymentConfiguration where
  rnf RequestPaymentConfiguration' {..} =
    Prelude.rnf payer

instance Data.ToXML RequestPaymentConfiguration where
  toXML RequestPaymentConfiguration' {..} =
    Prelude.mconcat ["Payer" Data.@= payer]
