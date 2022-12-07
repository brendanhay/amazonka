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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The Amazon S3 bucket prefix.
    bucketPrefix :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket.
    bucketName :: Prelude.Maybe Prelude.Text
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
-- 'bucketPrefix', 'successResponseHandlingConfig_bucketPrefix' - The Amazon S3 bucket prefix.
--
-- 'bucketName', 'successResponseHandlingConfig_bucketName' - The name of the Amazon S3 bucket.
newSuccessResponseHandlingConfig ::
  SuccessResponseHandlingConfig
newSuccessResponseHandlingConfig =
  SuccessResponseHandlingConfig'
    { bucketPrefix =
        Prelude.Nothing,
      bucketName = Prelude.Nothing
    }

-- | The Amazon S3 bucket prefix.
successResponseHandlingConfig_bucketPrefix :: Lens.Lens' SuccessResponseHandlingConfig (Prelude.Maybe Prelude.Text)
successResponseHandlingConfig_bucketPrefix = Lens.lens (\SuccessResponseHandlingConfig' {bucketPrefix} -> bucketPrefix) (\s@SuccessResponseHandlingConfig' {} a -> s {bucketPrefix = a} :: SuccessResponseHandlingConfig)

-- | The name of the Amazon S3 bucket.
successResponseHandlingConfig_bucketName :: Lens.Lens' SuccessResponseHandlingConfig (Prelude.Maybe Prelude.Text)
successResponseHandlingConfig_bucketName = Lens.lens (\SuccessResponseHandlingConfig' {bucketName} -> bucketName) (\s@SuccessResponseHandlingConfig' {} a -> s {bucketName = a} :: SuccessResponseHandlingConfig)

instance Data.FromJSON SuccessResponseHandlingConfig where
  parseJSON =
    Data.withObject
      "SuccessResponseHandlingConfig"
      ( \x ->
          SuccessResponseHandlingConfig'
            Prelude.<$> (x Data..:? "bucketPrefix")
            Prelude.<*> (x Data..:? "bucketName")
      )

instance
  Prelude.Hashable
    SuccessResponseHandlingConfig
  where
  hashWithSalt _salt SuccessResponseHandlingConfig' {..} =
    _salt `Prelude.hashWithSalt` bucketPrefix
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData SuccessResponseHandlingConfig where
  rnf SuccessResponseHandlingConfig' {..} =
    Prelude.rnf bucketPrefix
      `Prelude.seq` Prelude.rnf bucketName

instance Data.ToJSON SuccessResponseHandlingConfig where
  toJSON SuccessResponseHandlingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bucketPrefix" Data..=) Prelude.<$> bucketPrefix,
            ("bucketName" Data..=) Prelude.<$> bucketName
          ]
      )
