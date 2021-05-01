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
-- Module      : Network.AWS.SageMaker.Types.DataDistributionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataDistributionType
  ( DataDistributionType
      ( ..,
        DataDistributionType_FullyReplicated,
        DataDistributionType_ShardedByS3Key
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DataDistributionType = DataDistributionType'
  { fromDataDistributionType ::
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

pattern DataDistributionType_FullyReplicated :: DataDistributionType
pattern DataDistributionType_FullyReplicated = DataDistributionType' "FullyReplicated"

pattern DataDistributionType_ShardedByS3Key :: DataDistributionType
pattern DataDistributionType_ShardedByS3Key = DataDistributionType' "ShardedByS3Key"

{-# COMPLETE
  DataDistributionType_FullyReplicated,
  DataDistributionType_ShardedByS3Key,
  DataDistributionType'
  #-}
