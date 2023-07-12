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
-- Module      : Amazonka.AppFlow.Types.ErrorHandlingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ErrorHandlingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The settings that determine how Amazon AppFlow handles an error when
-- placing data in the destination. For example, this setting would
-- determine if the flow should fail after one insertion error, or continue
-- and attempt to insert every record regardless of the initial failure.
-- @ErrorHandlingConfig@ is a part of the destination connector details.
--
-- /See:/ 'newErrorHandlingConfig' smart constructor.
data ErrorHandlingConfig = ErrorHandlingConfig'
  { -- | Specifies the name of the Amazon S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon S3 bucket prefix.
    bucketPrefix :: Prelude.Maybe Prelude.Text,
    -- | Specifies if the flow should fail after the first instance of a failure
    -- when attempting to place data in the destination.
    failOnFirstDestinationError :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorHandlingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'errorHandlingConfig_bucketName' - Specifies the name of the Amazon S3 bucket.
--
-- 'bucketPrefix', 'errorHandlingConfig_bucketPrefix' - Specifies the Amazon S3 bucket prefix.
--
-- 'failOnFirstDestinationError', 'errorHandlingConfig_failOnFirstDestinationError' - Specifies if the flow should fail after the first instance of a failure
-- when attempting to place data in the destination.
newErrorHandlingConfig ::
  ErrorHandlingConfig
newErrorHandlingConfig =
  ErrorHandlingConfig'
    { bucketName = Prelude.Nothing,
      bucketPrefix = Prelude.Nothing,
      failOnFirstDestinationError = Prelude.Nothing
    }

-- | Specifies the name of the Amazon S3 bucket.
errorHandlingConfig_bucketName :: Lens.Lens' ErrorHandlingConfig (Prelude.Maybe Prelude.Text)
errorHandlingConfig_bucketName = Lens.lens (\ErrorHandlingConfig' {bucketName} -> bucketName) (\s@ErrorHandlingConfig' {} a -> s {bucketName = a} :: ErrorHandlingConfig)

-- | Specifies the Amazon S3 bucket prefix.
errorHandlingConfig_bucketPrefix :: Lens.Lens' ErrorHandlingConfig (Prelude.Maybe Prelude.Text)
errorHandlingConfig_bucketPrefix = Lens.lens (\ErrorHandlingConfig' {bucketPrefix} -> bucketPrefix) (\s@ErrorHandlingConfig' {} a -> s {bucketPrefix = a} :: ErrorHandlingConfig)

-- | Specifies if the flow should fail after the first instance of a failure
-- when attempting to place data in the destination.
errorHandlingConfig_failOnFirstDestinationError :: Lens.Lens' ErrorHandlingConfig (Prelude.Maybe Prelude.Bool)
errorHandlingConfig_failOnFirstDestinationError = Lens.lens (\ErrorHandlingConfig' {failOnFirstDestinationError} -> failOnFirstDestinationError) (\s@ErrorHandlingConfig' {} a -> s {failOnFirstDestinationError = a} :: ErrorHandlingConfig)

instance Data.FromJSON ErrorHandlingConfig where
  parseJSON =
    Data.withObject
      "ErrorHandlingConfig"
      ( \x ->
          ErrorHandlingConfig'
            Prelude.<$> (x Data..:? "bucketName")
            Prelude.<*> (x Data..:? "bucketPrefix")
            Prelude.<*> (x Data..:? "failOnFirstDestinationError")
      )

instance Prelude.Hashable ErrorHandlingConfig where
  hashWithSalt _salt ErrorHandlingConfig' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` bucketPrefix
      `Prelude.hashWithSalt` failOnFirstDestinationError

instance Prelude.NFData ErrorHandlingConfig where
  rnf ErrorHandlingConfig' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf bucketPrefix
      `Prelude.seq` Prelude.rnf failOnFirstDestinationError

instance Data.ToJSON ErrorHandlingConfig where
  toJSON ErrorHandlingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucketName" Data..=) Prelude.<$> bucketName,
            ("bucketPrefix" Data..=) Prelude.<$> bucketPrefix,
            ("failOnFirstDestinationError" Data..=)
              Prelude.<$> failOnFirstDestinationError
          ]
      )
