{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationStatus where

import Network.AWS.Prelude

data QualificationStatus
  = Granted
  | Revoked
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

instance FromText QualificationStatus where
  parser =
    takeLowerText >>= \case
      "granted" -> pure Granted
      "revoked" -> pure Revoked
      e ->
        fromTextError $
          "Failure parsing QualificationStatus from value: '" <> e
            <> "'. Accepted values: granted, revoked"

instance ToText QualificationStatus where
  toText = \case
    Granted -> "Granted"
    Revoked -> "Revoked"

instance Hashable QualificationStatus

instance NFData QualificationStatus

instance ToByteString QualificationStatus

instance ToQuery QualificationStatus

instance ToHeader QualificationStatus

instance ToJSON QualificationStatus where
  toJSON = toJSONText

instance FromJSON QualificationStatus where
  parseJSON = parseJSONText "QualificationStatus"
