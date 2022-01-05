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
-- Module      : Amazonka.S3.Types.BucketLoggingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.BucketLoggingStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.LoggingEnabled

-- | Container for logging status information.
--
-- /See:/ 'newBucketLoggingStatus' smart constructor.
data BucketLoggingStatus = BucketLoggingStatus'
  { loggingEnabled :: Prelude.Maybe LoggingEnabled
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketLoggingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingEnabled', 'bucketLoggingStatus_loggingEnabled' - Undocumented member.
newBucketLoggingStatus ::
  BucketLoggingStatus
newBucketLoggingStatus =
  BucketLoggingStatus'
    { loggingEnabled =
        Prelude.Nothing
    }

-- | Undocumented member.
bucketLoggingStatus_loggingEnabled :: Lens.Lens' BucketLoggingStatus (Prelude.Maybe LoggingEnabled)
bucketLoggingStatus_loggingEnabled = Lens.lens (\BucketLoggingStatus' {loggingEnabled} -> loggingEnabled) (\s@BucketLoggingStatus' {} a -> s {loggingEnabled = a} :: BucketLoggingStatus)

instance Prelude.Hashable BucketLoggingStatus where
  hashWithSalt _salt BucketLoggingStatus' {..} =
    _salt `Prelude.hashWithSalt` loggingEnabled

instance Prelude.NFData BucketLoggingStatus where
  rnf BucketLoggingStatus' {..} =
    Prelude.rnf loggingEnabled

instance Core.ToXML BucketLoggingStatus where
  toXML BucketLoggingStatus' {..} =
    Prelude.mconcat
      ["LoggingEnabled" Core.@= loggingEnabled]
