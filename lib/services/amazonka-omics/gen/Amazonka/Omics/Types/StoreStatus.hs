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
-- Module      : Amazonka.Omics.Types.StoreStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.StoreStatus
  ( StoreStatus
      ( ..,
        StoreStatus_ACTIVE,
        StoreStatus_CREATING,
        StoreStatus_DELETING,
        StoreStatus_FAILED,
        StoreStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StoreStatus = StoreStatus'
  { fromStoreStatus ::
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

pattern StoreStatus_ACTIVE :: StoreStatus
pattern StoreStatus_ACTIVE = StoreStatus' "ACTIVE"

pattern StoreStatus_CREATING :: StoreStatus
pattern StoreStatus_CREATING = StoreStatus' "CREATING"

pattern StoreStatus_DELETING :: StoreStatus
pattern StoreStatus_DELETING = StoreStatus' "DELETING"

pattern StoreStatus_FAILED :: StoreStatus
pattern StoreStatus_FAILED = StoreStatus' "FAILED"

pattern StoreStatus_UPDATING :: StoreStatus
pattern StoreStatus_UPDATING = StoreStatus' "UPDATING"

{-# COMPLETE
  StoreStatus_ACTIVE,
  StoreStatus_CREATING,
  StoreStatus_DELETING,
  StoreStatus_FAILED,
  StoreStatus_UPDATING,
  StoreStatus'
  #-}
