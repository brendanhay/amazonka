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
-- Module      : Network.AWS.AppSync.Types.ApiCacheStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ApiCacheStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype ApiCacheStatus = ApiCacheStatus'
  { fromApiCacheStatus ::
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
