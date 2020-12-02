{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotificationTransport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.NotificationTransport where

import Network.AWS.Prelude

data NotificationTransport
  = Email
  | SNS
  | Sqs
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText NotificationTransport where
  parser =
    takeLowerText >>= \case
      "email" -> pure Email
      "sns" -> pure SNS
      "sqs" -> pure Sqs
      e ->
        fromTextError $
          "Failure parsing NotificationTransport from value: '" <> e
            <> "'. Accepted values: email, sns, sqs"

instance ToText NotificationTransport where
  toText = \case
    Email -> "Email"
    SNS -> "SNS"
    Sqs -> "SQS"

instance Hashable NotificationTransport

instance NFData NotificationTransport

instance ToByteString NotificationTransport

instance ToQuery NotificationTransport

instance ToHeader NotificationTransport

instance ToJSON NotificationTransport where
  toJSON = toJSONText
