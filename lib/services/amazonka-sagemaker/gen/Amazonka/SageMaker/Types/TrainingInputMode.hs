{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.TrainingInputMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrainingInputMode
  ( TrainingInputMode
      ( ..,
        TrainingInputMode_FastFile,
        TrainingInputMode_File,
        TrainingInputMode_Pipe
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The training input mode that the algorithm supports. For more
-- information about input modes, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
--
-- __Pipe mode__
--
-- If an algorithm supports @Pipe@ mode, Amazon SageMaker streams data
-- directly from Amazon S3 to the container.
--
-- __File mode__
--
-- If an algorithm supports @File@ mode, SageMaker downloads the training
-- data from S3 to the provisioned ML storage volume, and mounts the
-- directory to the Docker volume for the training container.
--
-- You must provision the ML storage volume with sufficient capacity to
-- accommodate the data downloaded from S3. In addition to the training
-- data, the ML storage volume also stores the output model. The algorithm
-- container uses the ML storage volume to also store intermediate
-- information, if any.
--
-- For distributed algorithms, training data is distributed uniformly. Your
-- training duration is predictable if the input data objects sizes are
-- approximately the same. SageMaker does not split the files any further
-- for model training. If the object sizes are skewed, training won\'t be
-- optimal as the data distribution is also skewed when one host in a
-- training cluster is overloaded, thus becoming a bottleneck in training.
--
-- __FastFile mode__
--
-- If an algorithm supports @FastFile@ mode, SageMaker streams data
-- directly from S3 to the container with no code changes, and provides
-- file system access to the data. Users can author their training script
-- to interact with these files as if they were stored on disk.
--
-- @FastFile@ mode works best when the data is read sequentially. Augmented
-- manifest files aren\'t supported. The startup time is lower when there
-- are fewer files in the S3 bucket provided.
newtype TrainingInputMode = TrainingInputMode'
  { fromTrainingInputMode ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern TrainingInputMode_FastFile :: TrainingInputMode
pattern TrainingInputMode_FastFile = TrainingInputMode' "FastFile"

pattern TrainingInputMode_File :: TrainingInputMode
pattern TrainingInputMode_File = TrainingInputMode' "File"

pattern TrainingInputMode_Pipe :: TrainingInputMode
pattern TrainingInputMode_Pipe = TrainingInputMode' "Pipe"

{-# COMPLETE
  TrainingInputMode_FastFile,
  TrainingInputMode_File,
  TrainingInputMode_Pipe,
  TrainingInputMode'
  #-}
