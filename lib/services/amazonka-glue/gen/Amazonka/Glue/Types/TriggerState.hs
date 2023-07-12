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
-- Module      : Amazonka.Glue.Types.TriggerState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TriggerState
  ( TriggerState
      ( ..,
        TriggerState_ACTIVATED,
        TriggerState_ACTIVATING,
        TriggerState_CREATED,
        TriggerState_CREATING,
        TriggerState_DEACTIVATED,
        TriggerState_DEACTIVATING,
        TriggerState_DELETING,
        TriggerState_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TriggerState = TriggerState'
  { fromTriggerState ::
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

pattern TriggerState_ACTIVATED :: TriggerState
pattern TriggerState_ACTIVATED = TriggerState' "ACTIVATED"

pattern TriggerState_ACTIVATING :: TriggerState
pattern TriggerState_ACTIVATING = TriggerState' "ACTIVATING"

pattern TriggerState_CREATED :: TriggerState
pattern TriggerState_CREATED = TriggerState' "CREATED"

pattern TriggerState_CREATING :: TriggerState
pattern TriggerState_CREATING = TriggerState' "CREATING"

pattern TriggerState_DEACTIVATED :: TriggerState
pattern TriggerState_DEACTIVATED = TriggerState' "DEACTIVATED"

pattern TriggerState_DEACTIVATING :: TriggerState
pattern TriggerState_DEACTIVATING = TriggerState' "DEACTIVATING"

pattern TriggerState_DELETING :: TriggerState
pattern TriggerState_DELETING = TriggerState' "DELETING"

pattern TriggerState_UPDATING :: TriggerState
pattern TriggerState_UPDATING = TriggerState' "UPDATING"

{-# COMPLETE
  TriggerState_ACTIVATED,
  TriggerState_ACTIVATING,
  TriggerState_CREATED,
  TriggerState_CREATING,
  TriggerState_DEACTIVATED,
  TriggerState_DEACTIVATING,
  TriggerState_DELETING,
  TriggerState_UPDATING,
  TriggerState'
  #-}
