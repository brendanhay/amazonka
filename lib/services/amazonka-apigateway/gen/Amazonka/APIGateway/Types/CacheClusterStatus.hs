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
-- Module      : Amazonka.APIGateway.Types.CacheClusterStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.APIGateway.Types.CacheClusterStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns the status of the CacheCluster.
newtype CacheClusterStatus = CacheClusterStatus'
  { fromCacheClusterStatus ::
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
