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
-- Module      : Network.AWS.Chime.Types.NotificationTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.NotificationTarget
  ( NotificationTarget
      ( ..,
        NotificationTarget_EventBridge,
        NotificationTarget_SNS,
        NotificationTarget_SQS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype NotificationTarget = NotificationTarget'
  { fromNotificationTarget ::
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

pattern NotificationTarget_EventBridge :: NotificationTarget
pattern NotificationTarget_EventBridge = NotificationTarget' "EventBridge"

pattern NotificationTarget_SNS :: NotificationTarget
pattern NotificationTarget_SNS = NotificationTarget' "SNS"

pattern NotificationTarget_SQS :: NotificationTarget
pattern NotificationTarget_SQS = NotificationTarget' "SQS"

{-# COMPLETE
  NotificationTarget_EventBridge,
  NotificationTarget_SNS,
  NotificationTarget_SQS,
  NotificationTarget'
  #-}
