{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ServerCertificateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ServerCertificateStatus where

import Network.AWS.Prelude

data ServerCertificateStatus
  = Invalid
  | Valid
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

instance FromText ServerCertificateStatus where
  parser =
    takeLowerText >>= \case
      "invalid" -> pure Invalid
      "valid" -> pure Valid
      e ->
        fromTextError $
          "Failure parsing ServerCertificateStatus from value: '" <> e
            <> "'. Accepted values: invalid, valid"

instance ToText ServerCertificateStatus where
  toText = \case
    Invalid -> "INVALID"
    Valid -> "VALID"

instance Hashable ServerCertificateStatus

instance NFData ServerCertificateStatus

instance ToByteString ServerCertificateStatus

instance ToQuery ServerCertificateStatus

instance ToHeader ServerCertificateStatus

instance FromJSON ServerCertificateStatus where
  parseJSON = parseJSONText "ServerCertificateStatus"
