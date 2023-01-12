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
-- Module      : Amazonka.StorageGateway.Types.ActiveDirectoryStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.ActiveDirectoryStatus
  ( ActiveDirectoryStatus
      ( ..,
        ActiveDirectoryStatus_ACCESS_DENIED,
        ActiveDirectoryStatus_DETACHED,
        ActiveDirectoryStatus_JOINED,
        ActiveDirectoryStatus_JOINING,
        ActiveDirectoryStatus_NETWORK_ERROR,
        ActiveDirectoryStatus_TIMEOUT,
        ActiveDirectoryStatus_UNKNOWN_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActiveDirectoryStatus = ActiveDirectoryStatus'
  { fromActiveDirectoryStatus ::
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

pattern ActiveDirectoryStatus_ACCESS_DENIED :: ActiveDirectoryStatus
pattern ActiveDirectoryStatus_ACCESS_DENIED = ActiveDirectoryStatus' "ACCESS_DENIED"

pattern ActiveDirectoryStatus_DETACHED :: ActiveDirectoryStatus
pattern ActiveDirectoryStatus_DETACHED = ActiveDirectoryStatus' "DETACHED"

pattern ActiveDirectoryStatus_JOINED :: ActiveDirectoryStatus
pattern ActiveDirectoryStatus_JOINED = ActiveDirectoryStatus' "JOINED"

pattern ActiveDirectoryStatus_JOINING :: ActiveDirectoryStatus
pattern ActiveDirectoryStatus_JOINING = ActiveDirectoryStatus' "JOINING"

pattern ActiveDirectoryStatus_NETWORK_ERROR :: ActiveDirectoryStatus
pattern ActiveDirectoryStatus_NETWORK_ERROR = ActiveDirectoryStatus' "NETWORK_ERROR"

pattern ActiveDirectoryStatus_TIMEOUT :: ActiveDirectoryStatus
pattern ActiveDirectoryStatus_TIMEOUT = ActiveDirectoryStatus' "TIMEOUT"

pattern ActiveDirectoryStatus_UNKNOWN_ERROR :: ActiveDirectoryStatus
pattern ActiveDirectoryStatus_UNKNOWN_ERROR = ActiveDirectoryStatus' "UNKNOWN_ERROR"

{-# COMPLETE
  ActiveDirectoryStatus_ACCESS_DENIED,
  ActiveDirectoryStatus_DETACHED,
  ActiveDirectoryStatus_JOINED,
  ActiveDirectoryStatus_JOINING,
  ActiveDirectoryStatus_NETWORK_ERROR,
  ActiveDirectoryStatus_TIMEOUT,
  ActiveDirectoryStatus_UNKNOWN_ERROR,
  ActiveDirectoryStatus'
  #-}
