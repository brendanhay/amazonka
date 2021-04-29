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
-- Module      : Network.AWS.SageMaker.Types.ModelArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelArtifacts where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about the location that is configured for storing
-- model artifacts.
--
-- Model artifacts are the output that results from training a model, and
-- typically consist of trained parameters, a model defintion that desribes
-- how to compute inferences, and other metadata.
--
-- /See:/ 'newModelArtifacts' smart constructor.
data ModelArtifacts = ModelArtifacts'
  { -- | The path of the S3 object that contains the model artifacts. For
    -- example, @s3:\/\/bucket-name\/keynameprefix\/model.tar.gz@.
    s3ModelArtifacts :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModelArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3ModelArtifacts', 'modelArtifacts_s3ModelArtifacts' - The path of the S3 object that contains the model artifacts. For
-- example, @s3:\/\/bucket-name\/keynameprefix\/model.tar.gz@.
newModelArtifacts ::
  -- | 's3ModelArtifacts'
  Prelude.Text ->
  ModelArtifacts
newModelArtifacts pS3ModelArtifacts_ =
  ModelArtifacts'
    { s3ModelArtifacts =
        pS3ModelArtifacts_
    }

-- | The path of the S3 object that contains the model artifacts. For
-- example, @s3:\/\/bucket-name\/keynameprefix\/model.tar.gz@.
modelArtifacts_s3ModelArtifacts :: Lens.Lens' ModelArtifacts Prelude.Text
modelArtifacts_s3ModelArtifacts = Lens.lens (\ModelArtifacts' {s3ModelArtifacts} -> s3ModelArtifacts) (\s@ModelArtifacts' {} a -> s {s3ModelArtifacts = a} :: ModelArtifacts)

instance Prelude.FromJSON ModelArtifacts where
  parseJSON =
    Prelude.withObject
      "ModelArtifacts"
      ( \x ->
          ModelArtifacts'
            Prelude.<$> (x Prelude..: "S3ModelArtifacts")
      )

instance Prelude.Hashable ModelArtifacts

instance Prelude.NFData ModelArtifacts
