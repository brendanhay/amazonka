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
-- Module      : Network.AWS.APIGateway.Types.CacheClusterStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.CacheClusterStatus
  ( CacheClusterStatus
      ( ..,
        CacheClusterStatus_AVAILABLE,
        CacheClusterStatus_CREATE_IN_PROGRESS,
        CacheClusterStatus_DELETE_IN_PROGRESS,
        CacheClusterStatus_FLUSH_IN_PROGRESS,
        CacheClusterStatus_NOT_AVAILABLE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Returns the status of the __CacheCluster__.
newtype CacheClusterStatus = CacheClusterStatus'
  { fromCacheClusterStatus ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern CacheClusterStatus_AVAILABLE :: CacheClusterStatus
pattern CacheClusterStatus_AVAILABLE = CacheClusterStatus' "AVAILABLE"

pattern CacheClusterStatus_CREATE_IN_PROGRESS :: CacheClusterStatus
pattern CacheClusterStatus_CREATE_IN_PROGRESS = CacheClusterStatus' "CREATE_IN_PROGRESS"

pattern CacheClusterStatus_DELETE_IN_PROGRESS :: CacheClusterStatus
pattern CacheClusterStatus_DELETE_IN_PROGRESS = CacheClusterStatus' "DELETE_IN_PROGRESS"

pattern CacheClusterStatus_FLUSH_IN_PROGRESS :: CacheClusterStatus
pattern CacheClusterStatus_FLUSH_IN_PROGRESS = CacheClusterStatus' "FLUSH_IN_PROGRESS"

pattern CacheClusterStatus_NOT_AVAILABLE :: CacheClusterStatus
pattern CacheClusterStatus_NOT_AVAILABLE = CacheClusterStatus' "NOT_AVAILABLE"

{-# COMPLETE
  CacheClusterStatus_AVAILABLE,
  CacheClusterStatus_CREATE_IN_PROGRESS,
  CacheClusterStatus_DELETE_IN_PROGRESS,
  CacheClusterStatus_FLUSH_IN_PROGRESS,
  CacheClusterStatus_NOT_AVAILABLE,
  CacheClusterStatus'
  #-}
