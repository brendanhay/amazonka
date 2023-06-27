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
-- Module      : Amazonka.DataSync.Types.StorageSystemConnectivityStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.StorageSystemConnectivityStatus
  ( StorageSystemConnectivityStatus
      ( ..,
        StorageSystemConnectivityStatus_FAIL,
        StorageSystemConnectivityStatus_PASS,
        StorageSystemConnectivityStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StorageSystemConnectivityStatus = StorageSystemConnectivityStatus'
  { fromStorageSystemConnectivityStatus ::
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

pattern StorageSystemConnectivityStatus_FAIL :: StorageSystemConnectivityStatus
pattern StorageSystemConnectivityStatus_FAIL = StorageSystemConnectivityStatus' "FAIL"

pattern StorageSystemConnectivityStatus_PASS :: StorageSystemConnectivityStatus
pattern StorageSystemConnectivityStatus_PASS = StorageSystemConnectivityStatus' "PASS"

pattern StorageSystemConnectivityStatus_UNKNOWN :: StorageSystemConnectivityStatus
pattern StorageSystemConnectivityStatus_UNKNOWN = StorageSystemConnectivityStatus' "UNKNOWN"

{-# COMPLETE
  StorageSystemConnectivityStatus_FAIL,
  StorageSystemConnectivityStatus_PASS,
  StorageSystemConnectivityStatus_UNKNOWN,
  StorageSystemConnectivityStatus'
  #-}
