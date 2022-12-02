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
-- Module      : Amazonka.DevOpsGuru.Types.NotificationMessageType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.NotificationMessageType
  ( NotificationMessageType
      ( ..,
        NotificationMessageType_CLOSED_INSIGHT,
        NotificationMessageType_NEW_ASSOCIATION,
        NotificationMessageType_NEW_INSIGHT,
        NotificationMessageType_NEW_RECOMMENDATION,
        NotificationMessageType_SEVERITY_UPGRADED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NotificationMessageType = NotificationMessageType'
  { fromNotificationMessageType ::
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

pattern NotificationMessageType_CLOSED_INSIGHT :: NotificationMessageType
pattern NotificationMessageType_CLOSED_INSIGHT = NotificationMessageType' "CLOSED_INSIGHT"

pattern NotificationMessageType_NEW_ASSOCIATION :: NotificationMessageType
pattern NotificationMessageType_NEW_ASSOCIATION = NotificationMessageType' "NEW_ASSOCIATION"

pattern NotificationMessageType_NEW_INSIGHT :: NotificationMessageType
pattern NotificationMessageType_NEW_INSIGHT = NotificationMessageType' "NEW_INSIGHT"

pattern NotificationMessageType_NEW_RECOMMENDATION :: NotificationMessageType
pattern NotificationMessageType_NEW_RECOMMENDATION = NotificationMessageType' "NEW_RECOMMENDATION"

pattern NotificationMessageType_SEVERITY_UPGRADED :: NotificationMessageType
pattern NotificationMessageType_SEVERITY_UPGRADED = NotificationMessageType' "SEVERITY_UPGRADED"

{-# COMPLETE
  NotificationMessageType_CLOSED_INSIGHT,
  NotificationMessageType_NEW_ASSOCIATION,
  NotificationMessageType_NEW_INSIGHT,
  NotificationMessageType_NEW_RECOMMENDATION,
  NotificationMessageType_SEVERITY_UPGRADED,
  NotificationMessageType'
  #-}
