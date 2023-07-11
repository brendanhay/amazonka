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
-- Module      : Amazonka.Macie.Types.ClassificationTypeUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Types.ClassificationTypeUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Macie.Types.S3ContinuousClassificationType
import Amazonka.Macie.Types.S3OneTimeClassificationType
import qualified Amazonka.Prelude as Prelude

-- | (Discontinued) The classification type that Amazon Macie Classic applies
-- to the associated S3 resources. At least one of the classification types
-- (@oneTime@ or @continuous@) must be specified.
--
-- /See:/ 'newClassificationTypeUpdate' smart constructor.
data ClassificationTypeUpdate = ClassificationTypeUpdate'
  { -- | (Discontinued) A continuous classification of the objects that are added
    -- to a specified S3 bucket. Amazon Macie Classic begins performing
    -- continuous classification after a bucket is successfully associated with
    -- Macie Classic.
    continuous :: Prelude.Maybe S3ContinuousClassificationType,
    -- | (Discontinued) A one-time classification of all of the existing objects
    -- in a specified S3 bucket.
    oneTime :: Prelude.Maybe S3OneTimeClassificationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassificationTypeUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'continuous', 'classificationTypeUpdate_continuous' - (Discontinued) A continuous classification of the objects that are added
-- to a specified S3 bucket. Amazon Macie Classic begins performing
-- continuous classification after a bucket is successfully associated with
-- Macie Classic.
--
-- 'oneTime', 'classificationTypeUpdate_oneTime' - (Discontinued) A one-time classification of all of the existing objects
-- in a specified S3 bucket.
newClassificationTypeUpdate ::
  ClassificationTypeUpdate
newClassificationTypeUpdate =
  ClassificationTypeUpdate'
    { continuous =
        Prelude.Nothing,
      oneTime = Prelude.Nothing
    }

-- | (Discontinued) A continuous classification of the objects that are added
-- to a specified S3 bucket. Amazon Macie Classic begins performing
-- continuous classification after a bucket is successfully associated with
-- Macie Classic.
classificationTypeUpdate_continuous :: Lens.Lens' ClassificationTypeUpdate (Prelude.Maybe S3ContinuousClassificationType)
classificationTypeUpdate_continuous = Lens.lens (\ClassificationTypeUpdate' {continuous} -> continuous) (\s@ClassificationTypeUpdate' {} a -> s {continuous = a} :: ClassificationTypeUpdate)

-- | (Discontinued) A one-time classification of all of the existing objects
-- in a specified S3 bucket.
classificationTypeUpdate_oneTime :: Lens.Lens' ClassificationTypeUpdate (Prelude.Maybe S3OneTimeClassificationType)
classificationTypeUpdate_oneTime = Lens.lens (\ClassificationTypeUpdate' {oneTime} -> oneTime) (\s@ClassificationTypeUpdate' {} a -> s {oneTime = a} :: ClassificationTypeUpdate)

instance Prelude.Hashable ClassificationTypeUpdate where
  hashWithSalt _salt ClassificationTypeUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` continuous
      `Prelude.hashWithSalt` oneTime

instance Prelude.NFData ClassificationTypeUpdate where
  rnf ClassificationTypeUpdate' {..} =
    Prelude.rnf continuous
      `Prelude.seq` Prelude.rnf oneTime

instance Data.ToJSON ClassificationTypeUpdate where
  toJSON ClassificationTypeUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("continuous" Data..=) Prelude.<$> continuous,
            ("oneTime" Data..=) Prelude.<$> oneTime
          ]
      )
