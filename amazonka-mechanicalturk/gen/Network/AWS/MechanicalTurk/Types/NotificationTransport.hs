{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotificationTransport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.NotificationTransport
  ( NotificationTransport
      ( ..,
        NotificationTransport_Email,
        NotificationTransport_SNS,
        NotificationTransport_SQS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype NotificationTransport = NotificationTransport'
  { fromNotificationTransport ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
