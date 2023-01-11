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
-- Module      : Amazonka.Config.Types.MessageType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.MessageType
  ( MessageType
      ( ..,
        MessageType_ConfigurationItemChangeNotification,
        MessageType_ConfigurationSnapshotDeliveryCompleted,
        MessageType_OversizedConfigurationItemChangeNotification,
        MessageType_ScheduledNotification
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MessageType = MessageType'
  { fromMessageType ::
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
