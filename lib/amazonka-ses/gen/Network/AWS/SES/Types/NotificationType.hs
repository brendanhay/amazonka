{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.NotificationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.NotificationType where

import Network.AWS.Prelude

data NotificationType
  = Bounce
  | Complaint
  | Delivery
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

instance FromText NotificationType where
  parser =
    takeLowerText >>= \case
      "bounce" -> pure Bounce
      "complaint" -> pure Complaint
      "delivery" -> pure Delivery
      e ->
        fromTextError $
          "Failure parsing NotificationType from value: '" <> e
            <> "'. Accepted values: bounce, complaint, delivery"

instance ToText NotificationType where
  toText = \case
    Bounce -> "Bounce"
    Complaint -> "Complaint"
    Delivery -> "Delivery"

instance Hashable NotificationType

instance NFData NotificationType

instance ToByteString NotificationType

instance ToQuery NotificationType

instance ToHeader NotificationType
