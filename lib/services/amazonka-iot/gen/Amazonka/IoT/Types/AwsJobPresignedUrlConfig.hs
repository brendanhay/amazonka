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
-- Module      : Amazonka.IoT.Types.AwsJobPresignedUrlConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AwsJobPresignedUrlConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for pre-signed URLs. Valid when @protocols@
-- contains HTTP.
--
-- /See:/ 'newAwsJobPresignedUrlConfig' smart constructor.
data AwsJobPresignedUrlConfig = AwsJobPresignedUrlConfig'
  { -- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 -
    -- 3600, the default value is 1800 seconds. Pre-signed URLs are generated
    -- when a request for the job document is received.
    expiresInSec :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsJobPresignedUrlConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiresInSec', 'awsJobPresignedUrlConfig_expiresInSec' - How long (in seconds) pre-signed URLs are valid. Valid values are 60 -
-- 3600, the default value is 1800 seconds. Pre-signed URLs are generated
-- when a request for the job document is received.
newAwsJobPresignedUrlConfig ::
  AwsJobPresignedUrlConfig
newAwsJobPresignedUrlConfig =
  AwsJobPresignedUrlConfig'
    { expiresInSec =
        Prelude.Nothing
    }

-- | How long (in seconds) pre-signed URLs are valid. Valid values are 60 -
-- 3600, the default value is 1800 seconds. Pre-signed URLs are generated
-- when a request for the job document is received.
awsJobPresignedUrlConfig_expiresInSec :: Lens.Lens' AwsJobPresignedUrlConfig (Prelude.Maybe Prelude.Integer)
awsJobPresignedUrlConfig_expiresInSec = Lens.lens (\AwsJobPresignedUrlConfig' {expiresInSec} -> expiresInSec) (\s@AwsJobPresignedUrlConfig' {} a -> s {expiresInSec = a} :: AwsJobPresignedUrlConfig)

instance Data.FromJSON AwsJobPresignedUrlConfig where
  parseJSON =
    Data.withObject
      "AwsJobPresignedUrlConfig"
      ( \x ->
          AwsJobPresignedUrlConfig'
            Prelude.<$> (x Data..:? "expiresInSec")
      )

instance Prelude.Hashable AwsJobPresignedUrlConfig where
  hashWithSalt _salt AwsJobPresignedUrlConfig' {..} =
    _salt `Prelude.hashWithSalt` expiresInSec

instance Prelude.NFData AwsJobPresignedUrlConfig where
  rnf AwsJobPresignedUrlConfig' {..} =
    Prelude.rnf expiresInSec

instance Data.ToJSON AwsJobPresignedUrlConfig where
  toJSON AwsJobPresignedUrlConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [("expiresInSec" Data..=) Prelude.<$> expiresInSec]
      )
