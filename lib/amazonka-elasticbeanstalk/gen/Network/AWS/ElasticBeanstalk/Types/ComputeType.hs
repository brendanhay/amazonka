{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ComputeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ComputeType
  ( ComputeType
      ( ComputeType',
        ComputeTypeBuildGENERAL1Small,
        ComputeTypeBuildGENERAL1Medium,
        ComputeTypeBuildGENERAL1Large,
        fromComputeType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ComputeType = ComputeType' {fromComputeType :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ComputeTypeBuildGENERAL1Small :: ComputeType
pattern ComputeTypeBuildGENERAL1Small = ComputeType' "BUILD_GENERAL1_SMALL"

pattern ComputeTypeBuildGENERAL1Medium :: ComputeType
pattern ComputeTypeBuildGENERAL1Medium = ComputeType' "BUILD_GENERAL1_MEDIUM"

pattern ComputeTypeBuildGENERAL1Large :: ComputeType
pattern ComputeTypeBuildGENERAL1Large = ComputeType' "BUILD_GENERAL1_LARGE"

{-# COMPLETE
  ComputeTypeBuildGENERAL1Small,
  ComputeTypeBuildGENERAL1Medium,
  ComputeTypeBuildGENERAL1Large,
  ComputeType'
  #-}
