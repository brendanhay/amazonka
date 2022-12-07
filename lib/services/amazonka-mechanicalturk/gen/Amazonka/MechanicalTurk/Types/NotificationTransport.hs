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
-- Module      : Amazonka.MechanicalTurk.Types.NotificationTransport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.NotificationTransport
  ( NotificationTransport
      ( ..,
        NotificationTransport_Email,
        NotificationTransport_SNS,
        NotificationTransport_SQS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NotificationTransport = NotificationTransport'
  { fromNotificationTransport ::
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

pattern NotificationTransport_Email :: NotificationTransport
pattern NotificationTransport_Email = NotificationTransport' "Email"

pattern NotificationTransport_SNS :: NotificationTransport
pattern NotificationTransport_SNS = NotificationTransport' "SNS"

pattern NotificationTransport_SQS :: NotificationTransport
pattern NotificationTransport_SQS = NotificationTransport' "SQS"

{-# COMPLETE
  NotificationTransport_Email,
  NotificationTransport_SNS,
  NotificationTransport_SQS,
  NotificationTransport'
  #-}
