{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunNotificationSNSStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunNotificationSNSStatusCode where

import Network.AWS.Prelude

data AssessmentRunNotificationSNSStatusCode
  = AccessDenied
  | InternalError
  | Success
  | TopicDoesNotExist
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

instance FromText AssessmentRunNotificationSNSStatusCode where
  parser =
    takeLowerText >>= \case
      "access_denied" -> pure AccessDenied
      "internal_error" -> pure InternalError
      "success" -> pure Success
      "topic_does_not_exist" -> pure TopicDoesNotExist
      e ->
        fromTextError $
          "Failure parsing AssessmentRunNotificationSNSStatusCode from value: '" <> e
            <> "'. Accepted values: access_denied, internal_error, success, topic_does_not_exist"

instance ToText AssessmentRunNotificationSNSStatusCode where
  toText = \case
    AccessDenied -> "ACCESS_DENIED"
    InternalError -> "INTERNAL_ERROR"
    Success -> "SUCCESS"
    TopicDoesNotExist -> "TOPIC_DOES_NOT_EXIST"

instance Hashable AssessmentRunNotificationSNSStatusCode

instance NFData AssessmentRunNotificationSNSStatusCode

instance ToByteString AssessmentRunNotificationSNSStatusCode

instance ToQuery AssessmentRunNotificationSNSStatusCode

instance ToHeader AssessmentRunNotificationSNSStatusCode

instance FromJSON AssessmentRunNotificationSNSStatusCode where
  parseJSON = parseJSONText "AssessmentRunNotificationSNSStatusCode"
