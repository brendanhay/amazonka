{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingS3DataDistributionType
  ( ProcessingS3DataDistributionType
      ( ..,
        ProcessingS3DataDistributionType_FullyReplicated,
        ProcessingS3DataDistributionType_ShardedByS3Key
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ProcessingS3DataDistributionType = ProcessingS3DataDistributionType'
  { fromProcessingS3DataDistributionType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ProcessingS3DataDistributionType_FullyReplicated :: ProcessingS3DataDistributionType
pattern ProcessingS3DataDistributionType_FullyReplicated = ProcessingS3DataDistributionType' "FullyReplicated"

pattern ProcessingS3DataDistributionType_ShardedByS3Key :: ProcessingS3DataDistributionType
pattern ProcessingS3DataDistributionType_ShardedByS3Key = ProcessingS3DataDistributionType' "ShardedByS3Key"

{-# COMPLETE
  ProcessingS3DataDistributionType_FullyReplicated,
  ProcessingS3DataDistributionType_ShardedByS3Key,
  ProcessingS3DataDistributionType'
  #-}
