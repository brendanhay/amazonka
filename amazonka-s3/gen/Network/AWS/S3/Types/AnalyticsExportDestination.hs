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
-- Module      : Network.AWS.S3.Types.AnalyticsExportDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsExportDestination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsS3BucketDestination

-- | Where to publish the analytics results.
--
-- /See:/ 'newAnalyticsExportDestination' smart constructor.
data AnalyticsExportDestination = AnalyticsExportDestination'
  { -- | A destination signifying output to an S3 bucket.
    s3BucketDestination :: AnalyticsS3BucketDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AnalyticsExportDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketDestination', 'analyticsExportDestination_s3BucketDestination' - A destination signifying output to an S3 bucket.
newAnalyticsExportDestination ::
  -- | 's3BucketDestination'
  AnalyticsS3BucketDestination ->
  AnalyticsExportDestination
newAnalyticsExportDestination pS3BucketDestination_ =
  AnalyticsExportDestination'
    { s3BucketDestination =
        pS3BucketDestination_
    }

-- | A destination signifying output to an S3 bucket.
analyticsExportDestination_s3BucketDestination :: Lens.Lens' AnalyticsExportDestination AnalyticsS3BucketDestination
analyticsExportDestination_s3BucketDestination = Lens.lens (\AnalyticsExportDestination' {s3BucketDestination} -> s3BucketDestination) (\s@AnalyticsExportDestination' {} a -> s {s3BucketDestination = a} :: AnalyticsExportDestination)

instance Prelude.FromXML AnalyticsExportDestination where
  parseXML x =
    AnalyticsExportDestination'
      Prelude.<$> (x Prelude..@ "S3BucketDestination")

instance Prelude.Hashable AnalyticsExportDestination

instance Prelude.NFData AnalyticsExportDestination

instance Prelude.ToXML AnalyticsExportDestination where
  toXML AnalyticsExportDestination' {..} =
    Prelude.mconcat
      [ "S3BucketDestination"
          Prelude.@= s3BucketDestination
      ]
