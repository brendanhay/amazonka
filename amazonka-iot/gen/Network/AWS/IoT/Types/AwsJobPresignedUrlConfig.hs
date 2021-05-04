{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.AwsJobPresignedUrlConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AwsJobPresignedUrlConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AwsJobPresignedUrlConfig where
  parseJSON =
    Prelude.withObject
      "AwsJobPresignedUrlConfig"
      ( \x ->
          AwsJobPresignedUrlConfig'
            Prelude.<$> (x Prelude..:? "expiresInSec")
      )

instance Prelude.Hashable AwsJobPresignedUrlConfig

instance Prelude.NFData AwsJobPresignedUrlConfig

instance Prelude.ToJSON AwsJobPresignedUrlConfig where
  toJSON AwsJobPresignedUrlConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("expiresInSec" Prelude..=)
              Prelude.<$> expiresInSec
          ]
      )
