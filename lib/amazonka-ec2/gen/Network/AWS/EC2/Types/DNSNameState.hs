{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DNSNameState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DNSNameState where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data DNSNameState
  = DNSFailed
  | DNSPendingVerification
  | DNSVerified
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

instance FromText DNSNameState where
  parser =
    takeLowerText >>= \case
      "failed" -> pure DNSFailed
      "pendingverification" -> pure DNSPendingVerification
      "verified" -> pure DNSVerified
      e ->
        fromTextError $
          "Failure parsing DNSNameState from value: '" <> e
            <> "'. Accepted values: failed, pendingverification, verified"

instance ToText DNSNameState where
  toText = \case
    DNSFailed -> "failed"
    DNSPendingVerification -> "pendingVerification"
    DNSVerified -> "verified"

instance Hashable DNSNameState

instance NFData DNSNameState

instance ToByteString DNSNameState

instance ToQuery DNSNameState

instance ToHeader DNSNameState

instance FromXML DNSNameState where
  parseXML = parseXMLText "DNSNameState"
