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
-- Module      : Network.AWS.GuardDuty.Types.IpSetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.IpSetStatus
  ( IpSetStatus
      ( ..,
        IpSetStatus_ACTIVATING,
        IpSetStatus_ACTIVE,
        IpSetStatus_DEACTIVATING,
        IpSetStatus_DELETED,
        IpSetStatus_DELETE_PENDING,
        IpSetStatus_ERROR,
        IpSetStatus_INACTIVE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype IpSetStatus = IpSetStatus'
  { fromIpSetStatus ::
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

pattern IpSetStatus_ACTIVATING :: IpSetStatus
pattern IpSetStatus_ACTIVATING = IpSetStatus' "ACTIVATING"

pattern IpSetStatus_ACTIVE :: IpSetStatus
pattern IpSetStatus_ACTIVE = IpSetStatus' "ACTIVE"

pattern IpSetStatus_DEACTIVATING :: IpSetStatus
pattern IpSetStatus_DEACTIVATING = IpSetStatus' "DEACTIVATING"

pattern IpSetStatus_DELETED :: IpSetStatus
pattern IpSetStatus_DELETED = IpSetStatus' "DELETED"

pattern IpSetStatus_DELETE_PENDING :: IpSetStatus
pattern IpSetStatus_DELETE_PENDING = IpSetStatus' "DELETE_PENDING"

pattern IpSetStatus_ERROR :: IpSetStatus
pattern IpSetStatus_ERROR = IpSetStatus' "ERROR"

pattern IpSetStatus_INACTIVE :: IpSetStatus
pattern IpSetStatus_INACTIVE = IpSetStatus' "INACTIVE"

{-# COMPLETE
  IpSetStatus_ACTIVATING,
  IpSetStatus_ACTIVE,
  IpSetStatus_DEACTIVATING,
  IpSetStatus_DELETED,
  IpSetStatus_DELETE_PENDING,
  IpSetStatus_ERROR,
  IpSetStatus_INACTIVE,
  IpSetStatus'
  #-}
