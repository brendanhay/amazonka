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
-- Module      : Amazonka.SageMaker.Types.S3DataDistribution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.S3DataDistribution
  ( S3DataDistribution
      ( ..,
        S3DataDistribution_FullyReplicated,
        S3DataDistribution_ShardedByS3Key
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype S3DataDistribution = S3DataDistribution'
  { fromS3DataDistribution ::
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

pattern S3DataDistribution_FullyReplicated :: S3DataDistribution
pattern S3DataDistribution_FullyReplicated = S3DataDistribution' "FullyReplicated"

pattern S3DataDistribution_ShardedByS3Key :: S3DataDistribution
pattern S3DataDistribution_ShardedByS3Key = S3DataDistribution' "ShardedByS3Key"

{-# COMPLETE
  S3DataDistribution_FullyReplicated,
  S3DataDistribution_ShardedByS3Key,
  S3DataDistribution'
  #-}
