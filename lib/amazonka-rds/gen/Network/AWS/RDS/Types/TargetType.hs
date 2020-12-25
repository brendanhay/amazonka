{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetType
  ( TargetType
      ( TargetType',
        TargetTypeRdsInstance,
        TargetTypeRdsServerlessEndpoint,
        TargetTypeTrackedCluster,
        fromTargetType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TargetType = TargetType' {fromTargetType :: Core.Text}
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

pattern TargetTypeRdsInstance :: TargetType
pattern TargetTypeRdsInstance = TargetType' "RDS_INSTANCE"

pattern TargetTypeRdsServerlessEndpoint :: TargetType
pattern TargetTypeRdsServerlessEndpoint = TargetType' "RDS_SERVERLESS_ENDPOINT"

pattern TargetTypeTrackedCluster :: TargetType
pattern TargetTypeTrackedCluster = TargetType' "TRACKED_CLUSTER"

{-# COMPLETE
  TargetTypeRdsInstance,
  TargetTypeRdsServerlessEndpoint,
  TargetTypeTrackedCluster,
  TargetType'
  #-}
