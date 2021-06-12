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
-- Module      : Network.AWS.Config.Types.MessageType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MessageType
  ( MessageType
      ( ..,
        MessageType_ConfigurationItemChangeNotification,
        MessageType_ConfigurationSnapshotDeliveryCompleted,
        MessageType_OversizedConfigurationItemChangeNotification,
        MessageType_ScheduledNotification
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype MessageType = MessageType'
  { fromMessageType ::
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

pattern MessageType_ConfigurationItemChangeNotification :: MessageType
pattern MessageType_ConfigurationItemChangeNotification = MessageType' "ConfigurationItemChangeNotification"

pattern MessageType_ConfigurationSnapshotDeliveryCompleted :: MessageType
pattern MessageType_ConfigurationSnapshotDeliveryCompleted = MessageType' "ConfigurationSnapshotDeliveryCompleted"

pattern MessageType_OversizedConfigurationItemChangeNotification :: MessageType
pattern MessageType_OversizedConfigurationItemChangeNotification = MessageType' "OversizedConfigurationItemChangeNotification"

pattern MessageType_ScheduledNotification :: MessageType
pattern MessageType_ScheduledNotification = MessageType' "ScheduledNotification"

{-# COMPLETE
  MessageType_ConfigurationItemChangeNotification,
  MessageType_ConfigurationSnapshotDeliveryCompleted,
  MessageType_OversizedConfigurationItemChangeNotification,
  MessageType_ScheduledNotification,
  MessageType'
  #-}
