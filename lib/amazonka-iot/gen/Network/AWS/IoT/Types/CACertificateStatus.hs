{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CACertificateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CACertificateStatus where

import Network.AWS.Prelude

data CACertificateStatus
  = CACSActive
  | CACSInactive
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

instance FromText CACertificateStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure CACSActive
      "inactive" -> pure CACSInactive
      e ->
        fromTextError $
          "Failure parsing CACertificateStatus from value: '" <> e
            <> "'. Accepted values: active, inactive"

instance ToText CACertificateStatus where
  toText = \case
    CACSActive -> "ACTIVE"
    CACSInactive -> "INACTIVE"

instance Hashable CACertificateStatus

instance NFData CACertificateStatus

instance ToByteString CACertificateStatus

instance ToQuery CACertificateStatus

instance ToHeader CACertificateStatus

instance ToJSON CACertificateStatus where
  toJSON = toJSONText

instance FromJSON CACertificateStatus where
  parseJSON = parseJSONText "CACertificateStatus"
