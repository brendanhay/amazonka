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
-- Module      : Amazonka.Connect.Types.ActionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ActionType
  ( ActionType
      ( ..,
        ActionType_ASSIGN_CONTACT_CATEGORY,
        ActionType_CREATE_TASK,
        ActionType_GENERATE_EVENTBRIDGE_EVENT,
        ActionType_SEND_NOTIFICATION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionType = ActionType'
  { fromActionType ::
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

pattern ActionType_ASSIGN_CONTACT_CATEGORY :: ActionType
pattern ActionType_ASSIGN_CONTACT_CATEGORY = ActionType' "ASSIGN_CONTACT_CATEGORY"

pattern ActionType_CREATE_TASK :: ActionType
pattern ActionType_CREATE_TASK = ActionType' "CREATE_TASK"

pattern ActionType_GENERATE_EVENTBRIDGE_EVENT :: ActionType
pattern ActionType_GENERATE_EVENTBRIDGE_EVENT = ActionType' "GENERATE_EVENTBRIDGE_EVENT"

pattern ActionType_SEND_NOTIFICATION :: ActionType
pattern ActionType_SEND_NOTIFICATION = ActionType' "SEND_NOTIFICATION"

{-# COMPLETE
  ActionType_ASSIGN_CONTACT_CATEGORY,
  ActionType_CREATE_TASK,
  ActionType_GENERATE_EVENTBRIDGE_EVENT,
  ActionType_SEND_NOTIFICATION,
  ActionType'
  #-}
