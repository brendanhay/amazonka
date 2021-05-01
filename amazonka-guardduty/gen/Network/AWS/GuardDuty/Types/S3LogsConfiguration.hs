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
-- Module      : Network.AWS.GuardDuty.Types.S3LogsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.S3LogsConfiguration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes whether S3 data event logs will be enabled as a data source.
--
-- /See:/ 'newS3LogsConfiguration' smart constructor.
data S3LogsConfiguration = S3LogsConfiguration'
  { -- | The status of S3 data event logs as a data source.
    enable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3LogsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enable', 's3LogsConfiguration_enable' - The status of S3 data event logs as a data source.
newS3LogsConfiguration ::
  -- | 'enable'
  Prelude.Bool ->
  S3LogsConfiguration
newS3LogsConfiguration pEnable_ =
  S3LogsConfiguration' {enable = pEnable_}

-- | The status of S3 data event logs as a data source.
s3LogsConfiguration_enable :: Lens.Lens' S3LogsConfiguration Prelude.Bool
s3LogsConfiguration_enable = Lens.lens (\S3LogsConfiguration' {enable} -> enable) (\s@S3LogsConfiguration' {} a -> s {enable = a} :: S3LogsConfiguration)

instance Prelude.Hashable S3LogsConfiguration

instance Prelude.NFData S3LogsConfiguration

instance Prelude.ToJSON S3LogsConfiguration where
  toJSON S3LogsConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("enable" Prelude..= enable)]
      )
