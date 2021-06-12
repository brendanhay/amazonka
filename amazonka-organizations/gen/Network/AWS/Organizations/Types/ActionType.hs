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
-- Module      : Network.AWS.Organizations.Types.ActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.ActionType
  ( ActionType
      ( ..,
        ActionType_ADD_ORGANIZATIONS_SERVICE_LINKED_ROLE,
        ActionType_APPROVE_ALL_FEATURES,
        ActionType_ENABLE_ALL_FEATURES,
        ActionType_INVITE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ActionType = ActionType'
  { fromActionType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
