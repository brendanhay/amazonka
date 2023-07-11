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
-- Module      : Amazonka.AppFlow.Types.SuccessResponseHandlingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.SuccessResponseHandlingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Determines how Amazon AppFlow handles the success response that it gets
-- from the connector after placing data.
--
-- For example, this setting would determine where to write the response
-- from the destination connector upon a successful insert operation.
--
-- /See:/ 'newSuccessResponseHandlingConfig' smart constructor.
data SuccessResponseHandlingConfig = SuccessResponseHandlingConfig'
  { -- | The name of the Amazon S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket prefix.
    bucketPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuccessResponseHandlingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'successResponseHandlingConfig_bucketName' - The name of the Amazon S3 bucket.
--
-- 'bucketPrefix', 'successResponseHandlingConfig_bucketPrefix' - The Amazon S3 bucket prefix.
newSuccessResponseHandlingConfig ::
  SuccessResponseHandlingConfig
newSuccessResponseHandlingConfig =
  SuccessResponseHandlingConfig'
    { bucketName =
        Prelude.Nothing,
      bucketPrefix = Prelude.Nothing
    }

-- | The name of the Amazon S3 bucket.
successResponseHandlingConfig_bucketName :: Lens.Lens' SuccessResponseHandlingConfig (Prelude.Maybe Prelude.Text)
successResponseHandlingConfig_bucketName = Lens.lens (\SuccessResponseHandlingConfig' {bucketName} -> bucketName) (\s@SuccessResponseHandlingConfig' {} a -> s {bucketName = a} :: SuccessResponseHandlingConfig)

-- | The Amazon S3 bucket prefix.
successResponseHandlingConfig_bucketPrefix :: Lens.Lens' SuccessResponseHandlingConfig (Prelude.Maybe Prelude.Text)
successResponseHandlingConfig_bucketPrefix = Lens.lens (\SuccessResponseHandlingConfig' {bucketPrefix} -> bucketPrefix) (\s@SuccessResponseHandlingConfig' {} a -> s {bucketPrefix = a} :: SuccessResponseHandlingConfig)

instance Data.FromJSON SuccessResponseHandlingConfig where
  parseJSON =
    Data.withObject
      "SuccessResponseHandlingConfig"
      ( \x ->
          SuccessResponseHandlingConfig'
            Prelude.<$> (x Data..:? "bucketName")
            Prelude.<*> (x Data..:? "bucketPrefix")
      )

instance
  Prelude.Hashable
    SuccessResponseHandlingConfig
  where
  hashWithSalt _salt SuccessResponseHandlingConfig' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` bucketPrefix

instance Prelude.NFData SuccessResponseHandlingConfig where
  rnf SuccessResponseHandlingConfig' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf bucketPrefix

instance Data.ToJSON SuccessResponseHandlingConfig where
  toJSON SuccessResponseHandlingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucketName" Data..=) Prelude.<$> bucketName,
            ("bucketPrefix" Data..=) Prelude.<$> bucketPrefix
          ]
      )
