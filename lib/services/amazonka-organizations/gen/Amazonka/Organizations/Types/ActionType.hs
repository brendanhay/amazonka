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
-- Module      : Amazonka.Organizations.Types.ActionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.ActionType
  ( ActionType
      ( ..,
        ActionType_ADD_ORGANIZATIONS_SERVICE_LINKED_ROLE,
        ActionType_APPROVE_ALL_FEATURES,
        ActionType_ENABLE_ALL_FEATURES,
        ActionType_INVITE
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

pattern ActionType_ADD_ORGANIZATIONS_SERVICE_LINKED_ROLE :: ActionType
pattern ActionType_ADD_ORGANIZATIONS_SERVICE_LINKED_ROLE = ActionType' "ADD_ORGANIZATIONS_SERVICE_LINKED_ROLE"

pattern ActionType_APPROVE_ALL_FEATURES :: ActionType
pattern ActionType_APPROVE_ALL_FEATURES = ActionType' "APPROVE_ALL_FEATURES"

pattern ActionType_ENABLE_ALL_FEATURES :: ActionType
pattern ActionType_ENABLE_ALL_FEATURES = ActionType' "ENABLE_ALL_FEATURES"

pattern ActionType_INVITE :: ActionType
pattern ActionType_INVITE = ActionType' "INVITE"

{-# COMPLETE
  ActionType_ADD_ORGANIZATIONS_SERVICE_LINKED_ROLE,
  ActionType_APPROVE_ALL_FEATURES,
  ActionType_ENABLE_ALL_FEATURES,
  ActionType_INVITE,
  ActionType'
  #-}
