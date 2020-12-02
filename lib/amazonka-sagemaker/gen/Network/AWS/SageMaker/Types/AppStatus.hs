{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppStatus where

import Network.AWS.Prelude

data AppStatus
  = ASDeleted
  | ASDeleting
  | ASFailed
  | ASInService
  | ASPending
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

instance FromText AppStatus where
  parser =
    takeLowerText >>= \case
      "deleted" -> pure ASDeleted
      "deleting" -> pure ASDeleting
      "failed" -> pure ASFailed
      "inservice" -> pure ASInService
      "pending" -> pure ASPending
      e ->
        fromTextError $
          "Failure parsing AppStatus from value: '" <> e
            <> "'. Accepted values: deleted, deleting, failed, inservice, pending"

instance ToText AppStatus where
  toText = \case
    ASDeleted -> "Deleted"
    ASDeleting -> "Deleting"
    ASFailed -> "Failed"
    ASInService -> "InService"
    ASPending -> "Pending"

instance Hashable AppStatus

instance NFData AppStatus

instance ToByteString AppStatus

instance ToQuery AppStatus

instance ToHeader AppStatus

instance FromJSON AppStatus where
  parseJSON = parseJSONText "AppStatus"
