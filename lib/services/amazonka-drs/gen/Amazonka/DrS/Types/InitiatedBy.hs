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
-- Module      : Amazonka.DrS.Types.InitiatedBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.InitiatedBy
  ( InitiatedBy
      ( ..,
        InitiatedBy_ASSOCIATE_NETWORK_RECOVERY,
        InitiatedBy_CREATE_NETWORK_RECOVERY,
        InitiatedBy_DIAGNOSTIC,
        InitiatedBy_FAILBACK,
        InitiatedBy_START_DRILL,
        InitiatedBy_START_RECOVERY,
        InitiatedBy_TARGET_ACCOUNT,
        InitiatedBy_TERMINATE_RECOVERY_INSTANCES,
        InitiatedBy_UPDATE_NETWORK_RECOVERY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InitiatedBy = InitiatedBy'
  { fromInitiatedBy ::
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

pattern InitiatedBy_ASSOCIATE_NETWORK_RECOVERY :: InitiatedBy
pattern InitiatedBy_ASSOCIATE_NETWORK_RECOVERY = InitiatedBy' "ASSOCIATE_NETWORK_RECOVERY"

pattern InitiatedBy_CREATE_NETWORK_RECOVERY :: InitiatedBy
pattern InitiatedBy_CREATE_NETWORK_RECOVERY = InitiatedBy' "CREATE_NETWORK_RECOVERY"

pattern InitiatedBy_DIAGNOSTIC :: InitiatedBy
pattern InitiatedBy_DIAGNOSTIC = InitiatedBy' "DIAGNOSTIC"

pattern InitiatedBy_FAILBACK :: InitiatedBy
pattern InitiatedBy_FAILBACK = InitiatedBy' "FAILBACK"

pattern InitiatedBy_START_DRILL :: InitiatedBy
pattern InitiatedBy_START_DRILL = InitiatedBy' "START_DRILL"

pattern InitiatedBy_START_RECOVERY :: InitiatedBy
pattern InitiatedBy_START_RECOVERY = InitiatedBy' "START_RECOVERY"

pattern InitiatedBy_TARGET_ACCOUNT :: InitiatedBy
pattern InitiatedBy_TARGET_ACCOUNT = InitiatedBy' "TARGET_ACCOUNT"

pattern InitiatedBy_TERMINATE_RECOVERY_INSTANCES :: InitiatedBy
pattern InitiatedBy_TERMINATE_RECOVERY_INSTANCES = InitiatedBy' "TERMINATE_RECOVERY_INSTANCES"

pattern InitiatedBy_UPDATE_NETWORK_RECOVERY :: InitiatedBy
pattern InitiatedBy_UPDATE_NETWORK_RECOVERY = InitiatedBy' "UPDATE_NETWORK_RECOVERY"

{-# COMPLETE
  InitiatedBy_ASSOCIATE_NETWORK_RECOVERY,
  InitiatedBy_CREATE_NETWORK_RECOVERY,
  InitiatedBy_DIAGNOSTIC,
  InitiatedBy_FAILBACK,
  InitiatedBy_START_DRILL,
  InitiatedBy_START_RECOVERY,
  InitiatedBy_TARGET_ACCOUNT,
  InitiatedBy_TERMINATE_RECOVERY_INSTANCES,
  InitiatedBy_UPDATE_NETWORK_RECOVERY,
  InitiatedBy'
  #-}
