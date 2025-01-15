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
-- Module      : Amazonka.Macie.Types.FailedS3Resource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Types.FailedS3Resource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Macie.Types.S3Resource
import qualified Amazonka.Prelude as Prelude

-- | (Discontinued) Includes details about the failed S3 resources.
--
-- /See:/ 'newFailedS3Resource' smart constructor.
data FailedS3Resource = FailedS3Resource'
  { -- | (Discontinued) The status code of a failed item.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | (Discontinued) The error message of a failed item.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | (Discontinued) The failed S3 resources.
    failedItem :: Prelude.Maybe S3Resource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedS3Resource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorCode', 'failedS3Resource_errorCode' - (Discontinued) The status code of a failed item.
--
-- 'errorMessage', 'failedS3Resource_errorMessage' - (Discontinued) The error message of a failed item.
--
-- 'failedItem', 'failedS3Resource_failedItem' - (Discontinued) The failed S3 resources.
newFailedS3Resource ::
  FailedS3Resource
newFailedS3Resource =
  FailedS3Resource'
    { errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      failedItem = Prelude.Nothing
    }

-- | (Discontinued) The status code of a failed item.
failedS3Resource_errorCode :: Lens.Lens' FailedS3Resource (Prelude.Maybe Prelude.Text)
failedS3Resource_errorCode = Lens.lens (\FailedS3Resource' {errorCode} -> errorCode) (\s@FailedS3Resource' {} a -> s {errorCode = a} :: FailedS3Resource)

-- | (Discontinued) The error message of a failed item.
failedS3Resource_errorMessage :: Lens.Lens' FailedS3Resource (Prelude.Maybe Prelude.Text)
failedS3Resource_errorMessage = Lens.lens (\FailedS3Resource' {errorMessage} -> errorMessage) (\s@FailedS3Resource' {} a -> s {errorMessage = a} :: FailedS3Resource)

-- | (Discontinued) The failed S3 resources.
failedS3Resource_failedItem :: Lens.Lens' FailedS3Resource (Prelude.Maybe S3Resource)
failedS3Resource_failedItem = Lens.lens (\FailedS3Resource' {failedItem} -> failedItem) (\s@FailedS3Resource' {} a -> s {failedItem = a} :: FailedS3Resource)

instance Data.FromJSON FailedS3Resource where
  parseJSON =
    Data.withObject
      "FailedS3Resource"
      ( \x ->
          FailedS3Resource'
            Prelude.<$> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "failedItem")
      )

instance Prelude.Hashable FailedS3Resource where
  hashWithSalt _salt FailedS3Resource' {..} =
    _salt
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` failedItem

instance Prelude.NFData FailedS3Resource where
  rnf FailedS3Resource' {..} =
    Prelude.rnf errorCode `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf failedItem
