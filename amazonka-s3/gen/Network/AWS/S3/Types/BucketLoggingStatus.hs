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
-- Module      : Network.AWS.S3.Types.BucketLoggingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketLoggingStatus where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.LoggingEnabled

-- | Container for logging status information.
--
-- /See:/ 'newBucketLoggingStatus' smart constructor.
data BucketLoggingStatus = BucketLoggingStatus'
  { loggingEnabled :: Prelude.Maybe LoggingEnabled
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.Hashable BucketLoggingStatus

instance Prelude.NFData BucketLoggingStatus

instance Prelude.ToXML BucketLoggingStatus where
  toXML BucketLoggingStatus' {..} =
    Prelude.mconcat
      ["LoggingEnabled" Prelude.@= loggingEnabled]
