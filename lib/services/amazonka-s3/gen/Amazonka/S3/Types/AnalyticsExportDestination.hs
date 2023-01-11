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
-- Module      : Amazonka.S3.Types.AnalyticsExportDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.AnalyticsExportDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.AnalyticsS3BucketDestination

-- | Where to publish the analytics results.
--
-- /See:/ 'newAnalyticsExportDestination' smart constructor.
data AnalyticsExportDestination = AnalyticsExportDestination'
  { -- | A destination signifying output to an S3 bucket.
    s3BucketDestination :: AnalyticsS3BucketDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromXML AnalyticsExportDestination where
  parseXML x =
    AnalyticsExportDestination'
      Prelude.<$> (x Data..@ "S3BucketDestination")

instance Prelude.Hashable AnalyticsExportDestination where
  hashWithSalt _salt AnalyticsExportDestination' {..} =
    _salt `Prelude.hashWithSalt` s3BucketDestination

instance Prelude.NFData AnalyticsExportDestination where
  rnf AnalyticsExportDestination' {..} =
    Prelude.rnf s3BucketDestination

instance Data.ToXML AnalyticsExportDestination where
  toXML AnalyticsExportDestination' {..} =
    Prelude.mconcat
      ["S3BucketDestination" Data.@= s3BucketDestination]
