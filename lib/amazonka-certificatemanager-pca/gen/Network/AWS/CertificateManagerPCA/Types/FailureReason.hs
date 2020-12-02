{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.FailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.FailureReason where

import Network.AWS.Prelude

data FailureReason
  = Other
  | RequestTimedOut
  | UnsupportedAlgorithm
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

instance FromText FailureReason where
  parser =
    takeLowerText >>= \case
      "other" -> pure Other
      "request_timed_out" -> pure RequestTimedOut
      "unsupported_algorithm" -> pure UnsupportedAlgorithm
      e ->
        fromTextError $
          "Failure parsing FailureReason from value: '" <> e
            <> "'. Accepted values: other, request_timed_out, unsupported_algorithm"

instance ToText FailureReason where
  toText = \case
    Other -> "OTHER"
    RequestTimedOut -> "REQUEST_TIMED_OUT"
    UnsupportedAlgorithm -> "UNSUPPORTED_ALGORITHM"

instance Hashable FailureReason

instance NFData FailureReason

instance ToByteString FailureReason

instance ToQuery FailureReason

instance ToHeader FailureReason

instance FromJSON FailureReason where
  parseJSON = parseJSONText "FailureReason"
