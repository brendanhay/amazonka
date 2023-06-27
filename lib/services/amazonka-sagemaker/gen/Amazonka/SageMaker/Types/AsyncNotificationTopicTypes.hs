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
-- Module      : Amazonka.SageMaker.Types.AsyncNotificationTopicTypes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AsyncNotificationTopicTypes
  ( AsyncNotificationTopicTypes
      ( ..,
        AsyncNotificationTopicTypes_ERROR_NOTIFICATION_TOPIC,
        AsyncNotificationTopicTypes_SUCCESS_NOTIFICATION_TOPIC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AsyncNotificationTopicTypes = AsyncNotificationTopicTypes'
  { fromAsyncNotificationTopicTypes ::
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

pattern AsyncNotificationTopicTypes_ERROR_NOTIFICATION_TOPIC :: AsyncNotificationTopicTypes
pattern AsyncNotificationTopicTypes_ERROR_NOTIFICATION_TOPIC = AsyncNotificationTopicTypes' "ERROR_NOTIFICATION_TOPIC"

pattern AsyncNotificationTopicTypes_SUCCESS_NOTIFICATION_TOPIC :: AsyncNotificationTopicTypes
pattern AsyncNotificationTopicTypes_SUCCESS_NOTIFICATION_TOPIC = AsyncNotificationTopicTypes' "SUCCESS_NOTIFICATION_TOPIC"

{-# COMPLETE
  AsyncNotificationTopicTypes_ERROR_NOTIFICATION_TOPIC,
  AsyncNotificationTopicTypes_SUCCESS_NOTIFICATION_TOPIC,
  AsyncNotificationTopicTypes'
  #-}
