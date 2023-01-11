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
-- Module      : Amazonka.Translate.Types.OutputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.OutputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.EncryptionKey

-- | The output configuration properties for a batch translation job.
--
-- /See:/ 'newOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { encryptionKey :: Prelude.Maybe EncryptionKey,
    -- | The URI of the S3 folder that contains a translation job\'s output file.
    -- The folder must be in the same Region as the API endpoint that you are
    -- calling.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKey', 'outputDataConfig_encryptionKey' - Undocumented member.
--
-- 's3Uri', 'outputDataConfig_s3Uri' - The URI of the S3 folder that contains a translation job\'s output file.
-- The folder must be in the same Region as the API endpoint that you are
-- calling.
newOutputDataConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  OutputDataConfig
newOutputDataConfig pS3Uri_ =
  OutputDataConfig'
    { encryptionKey = Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | Undocumented member.
outputDataConfig_encryptionKey :: Lens.Lens' OutputDataConfig (Prelude.Maybe EncryptionKey)
outputDataConfig_encryptionKey = Lens.lens (\OutputDataConfig' {encryptionKey} -> encryptionKey) (\s@OutputDataConfig' {} a -> s {encryptionKey = a} :: OutputDataConfig)

-- | The URI of the S3 folder that contains a translation job\'s output file.
-- The folder must be in the same Region as the API endpoint that you are
-- calling.
outputDataConfig_s3Uri :: Lens.Lens' OutputDataConfig Prelude.Text
outputDataConfig_s3Uri = Lens.lens (\OutputDataConfig' {s3Uri} -> s3Uri) (\s@OutputDataConfig' {} a -> s {s3Uri = a} :: OutputDataConfig)

instance Data.FromJSON OutputDataConfig where
  parseJSON =
    Data.withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig'
            Prelude.<$> (x Data..:? "EncryptionKey")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable OutputDataConfig where
  hashWithSalt _salt OutputDataConfig' {..} =
    _salt `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData OutputDataConfig where
  rnf OutputDataConfig' {..} =
    Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EncryptionKey" Data..=) Prelude.<$> encryptionKey,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
