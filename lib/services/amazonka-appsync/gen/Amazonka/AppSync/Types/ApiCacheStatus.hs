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
-- Module      : Amazonka.AppSync.Types.ApiCacheStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.ApiCacheStatus
  ( ApiCacheStatus
      ( ..,
        ApiCacheStatus_AVAILABLE,
        ApiCacheStatus_CREATING,
        ApiCacheStatus_DELETING,
        ApiCacheStatus_FAILED,
        ApiCacheStatus_MODIFYING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApiCacheStatus = ApiCacheStatus'
  { fromApiCacheStatus ::
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

pattern ApiCacheStatus_AVAILABLE :: ApiCacheStatus
pattern ApiCacheStatus_AVAILABLE = ApiCacheStatus' "AVAILABLE"

pattern ApiCacheStatus_CREATING :: ApiCacheStatus
pattern ApiCacheStatus_CREATING = ApiCacheStatus' "CREATING"

pattern ApiCacheStatus_DELETING :: ApiCacheStatus
pattern ApiCacheStatus_DELETING = ApiCacheStatus' "DELETING"

pattern ApiCacheStatus_FAILED :: ApiCacheStatus
pattern ApiCacheStatus_FAILED = ApiCacheStatus' "FAILED"

pattern ApiCacheStatus_MODIFYING :: ApiCacheStatus
pattern ApiCacheStatus_MODIFYING = ApiCacheStatus' "MODIFYING"

{-# COMPLETE
  ApiCacheStatus_AVAILABLE,
  ApiCacheStatus_CREATING,
  ApiCacheStatus_DELETING,
  ApiCacheStatus_FAILED,
  ApiCacheStatus_MODIFYING,
  ApiCacheStatus'
  #-}
