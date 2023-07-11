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
-- Module      : Amazonka.GuardDuty.Types.IpSetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.IpSetStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IpSetStatus = IpSetStatus'
  { fromIpSetStatus ::
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
