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
-- Module      : Amazonka.Rekognition.Types.Summary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.Summary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.S3Object

-- | The S3 bucket that contains the training summary. The training summary
-- includes aggregated evaluation metrics for the entire testing dataset
-- and metrics for each individual label.
--
-- You get the training summary S3 bucket location by calling
-- DescribeProjectVersions.
--
-- /See:/ 'newSummary' smart constructor.
data Summary = Summary'
  { s3Object :: Prelude.Maybe S3Object
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Summary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Object', 'summary_s3Object' - Undocumented member.
newSummary ::
  Summary
newSummary = Summary' {s3Object = Prelude.Nothing}

-- | Undocumented member.
summary_s3Object :: Lens.Lens' Summary (Prelude.Maybe S3Object)
summary_s3Object = Lens.lens (\Summary' {s3Object} -> s3Object) (\s@Summary' {} a -> s {s3Object = a} :: Summary)

instance Data.FromJSON Summary where
  parseJSON =
    Data.withObject
      "Summary"
      (\x -> Summary' Prelude.<$> (x Data..:? "S3Object"))

instance Prelude.Hashable Summary where
  hashWithSalt _salt Summary' {..} =
    _salt `Prelude.hashWithSalt` s3Object

instance Prelude.NFData Summary where
  rnf Summary' {..} = Prelude.rnf s3Object
