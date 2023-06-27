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
-- Module      : Amazonka.RolesAnywhere.Types.NotificationEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.NotificationEvent
  ( NotificationEvent
      ( ..,
        NotificationEvent_CA_CERTIFICATE_EXPIRY,
        NotificationEvent_END_ENTITY_CERTIFICATE_EXPIRY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NotificationEvent = NotificationEvent'
  { fromNotificationEvent ::
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

pattern NotificationEvent_CA_CERTIFICATE_EXPIRY :: NotificationEvent
pattern NotificationEvent_CA_CERTIFICATE_EXPIRY = NotificationEvent' "CA_CERTIFICATE_EXPIRY"

pattern NotificationEvent_END_ENTITY_CERTIFICATE_EXPIRY :: NotificationEvent
pattern NotificationEvent_END_ENTITY_CERTIFICATE_EXPIRY = NotificationEvent' "END_ENTITY_CERTIFICATE_EXPIRY"

{-# COMPLETE
  NotificationEvent_CA_CERTIFICATE_EXPIRY,
  NotificationEvent_END_ENTITY_CERTIFICATE_EXPIRY,
  NotificationEvent'
  #-}
