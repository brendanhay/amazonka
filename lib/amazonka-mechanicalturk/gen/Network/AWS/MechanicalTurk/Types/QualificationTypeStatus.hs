{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.QualificationTypeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.QualificationTypeStatus where

import Network.AWS.Prelude

data QualificationTypeStatus
  = Active
  | Inactive
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

instance FromText QualificationTypeStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure Active
      "inactive" -> pure Inactive
      e ->
        fromTextError $
          "Failure parsing QualificationTypeStatus from value: '" <> e
            <> "'. Accepted values: active, inactive"

instance ToText QualificationTypeStatus where
  toText = \case
    Active -> "Active"
    Inactive -> "Inactive"

instance Hashable QualificationTypeStatus

instance NFData QualificationTypeStatus

instance ToByteString QualificationTypeStatus

instance ToQuery QualificationTypeStatus

instance ToHeader QualificationTypeStatus

instance ToJSON QualificationTypeStatus where
  toJSON = toJSONText

instance FromJSON QualificationTypeStatus where
  parseJSON = parseJSONText "QualificationTypeStatus"
