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
-- Module      : Network.AWS.SMS.Types.AppStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppStatus
  ( AppStatus
      ( ..,
        AppStatus_ACTIVE,
        AppStatus_CREATING,
        AppStatus_DELETED,
        AppStatus_DELETE_FAILED,
        AppStatus_DELETING,
        AppStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype AppStatus = AppStatus'
  { fromAppStatus ::
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

pattern AppStatus_ACTIVE :: AppStatus
pattern AppStatus_ACTIVE = AppStatus' "ACTIVE"

pattern AppStatus_CREATING :: AppStatus
pattern AppStatus_CREATING = AppStatus' "CREATING"

pattern AppStatus_DELETED :: AppStatus
pattern AppStatus_DELETED = AppStatus' "DELETED"

pattern AppStatus_DELETE_FAILED :: AppStatus
pattern AppStatus_DELETE_FAILED = AppStatus' "DELETE_FAILED"

pattern AppStatus_DELETING :: AppStatus
pattern AppStatus_DELETING = AppStatus' "DELETING"

pattern AppStatus_UPDATING :: AppStatus
pattern AppStatus_UPDATING = AppStatus' "UPDATING"

{-# COMPLETE
  AppStatus_ACTIVE,
  AppStatus_CREATING,
  AppStatus_DELETED,
  AppStatus_DELETE_FAILED,
  AppStatus_DELETING,
  AppStatus_UPDATING,
  AppStatus'
  #-}
