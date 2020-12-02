{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.CustomMailFromStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CustomMailFromStatus where

import Network.AWS.Prelude

data CustomMailFromStatus
  = CMFSFailed
  | CMFSPending
  | CMFSSuccess
  | CMFSTemporaryFailure
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

instance FromText CustomMailFromStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure CMFSFailed
      "pending" -> pure CMFSPending
      "success" -> pure CMFSSuccess
      "temporaryfailure" -> pure CMFSTemporaryFailure
      e ->
        fromTextError $
          "Failure parsing CustomMailFromStatus from value: '" <> e
            <> "'. Accepted values: failed, pending, success, temporaryfailure"

instance ToText CustomMailFromStatus where
  toText = \case
    CMFSFailed -> "Failed"
    CMFSPending -> "Pending"
    CMFSSuccess -> "Success"
    CMFSTemporaryFailure -> "TemporaryFailure"

instance Hashable CustomMailFromStatus

instance NFData CustomMailFromStatus

instance ToByteString CustomMailFromStatus

instance ToQuery CustomMailFromStatus

instance ToHeader CustomMailFromStatus

instance FromXML CustomMailFromStatus where
  parseXML = parseXMLText "CustomMailFromStatus"
