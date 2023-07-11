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
-- Module      : Amazonka.Macie.Types.ClassificationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Types.ClassificationType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Macie.Types.S3ContinuousClassificationType
import Amazonka.Macie.Types.S3OneTimeClassificationType
import qualified Amazonka.Prelude as Prelude

-- | (Discontinued) The classification type that Amazon Macie Classic applies
-- to the associated S3 resources.
--
-- /See:/ 'newClassificationType' smart constructor.
data ClassificationType = ClassificationType'
  { -- | (Discontinued) A one-time classification of all of the existing objects
    -- in a specified S3 bucket.
    oneTime :: S3OneTimeClassificationType,
    -- | (Discontinued) A continuous classification of the objects that are added
    -- to a specified S3 bucket. Amazon Macie Classic begins performing
    -- continuous classification after a bucket is successfully associated with
    -- Macie Classic.
    continuous :: S3ContinuousClassificationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassificationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oneTime', 'classificationType_oneTime' - (Discontinued) A one-time classification of all of the existing objects
-- in a specified S3 bucket.
--
-- 'continuous', 'classificationType_continuous' - (Discontinued) A continuous classification of the objects that are added
-- to a specified S3 bucket. Amazon Macie Classic begins performing
-- continuous classification after a bucket is successfully associated with
-- Macie Classic.
newClassificationType ::
  -- | 'oneTime'
  S3OneTimeClassificationType ->
  -- | 'continuous'
  S3ContinuousClassificationType ->
  ClassificationType
newClassificationType pOneTime_ pContinuous_ =
  ClassificationType'
    { oneTime = pOneTime_,
      continuous = pContinuous_
    }

-- | (Discontinued) A one-time classification of all of the existing objects
-- in a specified S3 bucket.
classificationType_oneTime :: Lens.Lens' ClassificationType S3OneTimeClassificationType
classificationType_oneTime = Lens.lens (\ClassificationType' {oneTime} -> oneTime) (\s@ClassificationType' {} a -> s {oneTime = a} :: ClassificationType)

-- | (Discontinued) A continuous classification of the objects that are added
-- to a specified S3 bucket. Amazon Macie Classic begins performing
-- continuous classification after a bucket is successfully associated with
-- Macie Classic.
classificationType_continuous :: Lens.Lens' ClassificationType S3ContinuousClassificationType
classificationType_continuous = Lens.lens (\ClassificationType' {continuous} -> continuous) (\s@ClassificationType' {} a -> s {continuous = a} :: ClassificationType)

instance Data.FromJSON ClassificationType where
  parseJSON =
    Data.withObject
      "ClassificationType"
      ( \x ->
          ClassificationType'
            Prelude.<$> (x Data..: "oneTime")
            Prelude.<*> (x Data..: "continuous")
      )

instance Prelude.Hashable ClassificationType where
  hashWithSalt _salt ClassificationType' {..} =
    _salt
      `Prelude.hashWithSalt` oneTime
      `Prelude.hashWithSalt` continuous

instance Prelude.NFData ClassificationType where
  rnf ClassificationType' {..} =
    Prelude.rnf oneTime
      `Prelude.seq` Prelude.rnf continuous

instance Data.ToJSON ClassificationType where
  toJSON ClassificationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("oneTime" Data..= oneTime),
            Prelude.Just ("continuous" Data..= continuous)
          ]
      )
