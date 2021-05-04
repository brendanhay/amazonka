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

import qualified Network.AWS.Prelude as Prelude

newtype DBProxyStatus = DBProxyStatus'
  { fromDBProxyStatus ::
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
