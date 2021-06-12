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
-- Module      : Network.AWS.RDS.Types.DBProxyStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxyStatus
  ( DBProxyStatus
      ( ..,
        DBProxyStatus_Available,
        DBProxyStatus_Creating,
        DBProxyStatus_Deleting,
        DBProxyStatus_Incompatible_network,
        DBProxyStatus_Insufficient_resource_limits,
        DBProxyStatus_Modifying,
        DBProxyStatus_Reactivating,
        DBProxyStatus_Suspended,
        DBProxyStatus_Suspending
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DBProxyStatus = DBProxyStatus'
  { fromDBProxyStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern DBProxyStatus_Available :: DBProxyStatus
pattern DBProxyStatus_Available = DBProxyStatus' "available"

pattern DBProxyStatus_Creating :: DBProxyStatus
pattern DBProxyStatus_Creating = DBProxyStatus' "creating"

pattern DBProxyStatus_Deleting :: DBProxyStatus
pattern DBProxyStatus_Deleting = DBProxyStatus' "deleting"

pattern DBProxyStatus_Incompatible_network :: DBProxyStatus
pattern DBProxyStatus_Incompatible_network = DBProxyStatus' "incompatible-network"

pattern DBProxyStatus_Insufficient_resource_limits :: DBProxyStatus
pattern DBProxyStatus_Insufficient_resource_limits = DBProxyStatus' "insufficient-resource-limits"

pattern DBProxyStatus_Modifying :: DBProxyStatus
pattern DBProxyStatus_Modifying = DBProxyStatus' "modifying"

pattern DBProxyStatus_Reactivating :: DBProxyStatus
pattern DBProxyStatus_Reactivating = DBProxyStatus' "reactivating"

pattern DBProxyStatus_Suspended :: DBProxyStatus
pattern DBProxyStatus_Suspended = DBProxyStatus' "suspended"

pattern DBProxyStatus_Suspending :: DBProxyStatus
pattern DBProxyStatus_Suspending = DBProxyStatus' "suspending"

{-# COMPLETE
  DBProxyStatus_Available,
  DBProxyStatus_Creating,
  DBProxyStatus_Deleting,
  DBProxyStatus_Incompatible_network,
  DBProxyStatus_Insufficient_resource_limits,
  DBProxyStatus_Modifying,
  DBProxyStatus_Reactivating,
  DBProxyStatus_Suspended,
  DBProxyStatus_Suspending,
  DBProxyStatus'
  #-}
