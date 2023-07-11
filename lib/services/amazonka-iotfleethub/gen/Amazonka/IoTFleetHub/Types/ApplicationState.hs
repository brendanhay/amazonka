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
-- Module      : Amazonka.IoTFleetHub.Types.ApplicationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetHub.Types.ApplicationState
  ( ApplicationState
      ( ..,
        ApplicationState_ACTIVE,
        ApplicationState_CREATE_FAILED,
        ApplicationState_CREATING,
        ApplicationState_DELETE_FAILED,
        ApplicationState_DELETING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ApplicationState = ApplicationState'
  { fromApplicationState ::
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

pattern ApplicationState_ACTIVE :: ApplicationState
pattern ApplicationState_ACTIVE = ApplicationState' "ACTIVE"

pattern ApplicationState_CREATE_FAILED :: ApplicationState
pattern ApplicationState_CREATE_FAILED = ApplicationState' "CREATE_FAILED"

pattern ApplicationState_CREATING :: ApplicationState
pattern ApplicationState_CREATING = ApplicationState' "CREATING"

pattern ApplicationState_DELETE_FAILED :: ApplicationState
pattern ApplicationState_DELETE_FAILED = ApplicationState' "DELETE_FAILED"

pattern ApplicationState_DELETING :: ApplicationState
pattern ApplicationState_DELETING = ApplicationState' "DELETING"

{-# COMPLETE
  ApplicationState_ACTIVE,
  ApplicationState_CREATE_FAILED,
  ApplicationState_CREATING,
  ApplicationState_DELETE_FAILED,
  ApplicationState_DELETING,
  ApplicationState'
  #-}
