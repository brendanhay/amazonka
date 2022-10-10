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
-- Module      : Amazonka.CodeStarNotifications.Types.NotificationRuleStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStarNotifications.Types.NotificationRuleStatus
  ( NotificationRuleStatus
      ( ..,
        NotificationRuleStatus_DISABLED,
        NotificationRuleStatus_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype NotificationRuleStatus = NotificationRuleStatus'
  { fromNotificationRuleStatus ::
      Core.Text
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

pattern NotificationRuleStatus_DISABLED :: NotificationRuleStatus
pattern NotificationRuleStatus_DISABLED = NotificationRuleStatus' "DISABLED"

pattern NotificationRuleStatus_ENABLED :: NotificationRuleStatus
pattern NotificationRuleStatus_ENABLED = NotificationRuleStatus' "ENABLED"

{-# COMPLETE
  NotificationRuleStatus_DISABLED,
  NotificationRuleStatus_ENABLED,
  NotificationRuleStatus'
  #-}
