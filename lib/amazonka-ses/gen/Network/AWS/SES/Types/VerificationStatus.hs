{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.VerificationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.VerificationStatus where

import Network.AWS.Prelude

data VerificationStatus
  = VSFailed
  | VSNotStarted
  | VSPending
  | VSSuccess
  | VSTemporaryFailure
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

instance FromText VerificationStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure VSFailed
      "notstarted" -> pure VSNotStarted
      "pending" -> pure VSPending
      "success" -> pure VSSuccess
      "temporaryfailure" -> pure VSTemporaryFailure
      e ->
        fromTextError $
          "Failure parsing VerificationStatus from value: '" <> e
            <> "'. Accepted values: failed, notstarted, pending, success, temporaryfailure"

instance ToText VerificationStatus where
  toText = \case
    VSFailed -> "Failed"
    VSNotStarted -> "NotStarted"
    VSPending -> "Pending"
    VSSuccess -> "Success"
    VSTemporaryFailure -> "TemporaryFailure"

instance Hashable VerificationStatus

instance NFData VerificationStatus

instance ToByteString VerificationStatus

instance ToQuery VerificationStatus

instance ToHeader VerificationStatus

instance FromXML VerificationStatus where
  parseXML = parseXMLText "VerificationStatus"
