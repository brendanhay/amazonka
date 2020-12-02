{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.CRType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.CRType where

import Network.AWS.Prelude

data CRType
  = EC2
  | Spot
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

instance FromText CRType where
  parser =
    takeLowerText >>= \case
      "ec2" -> pure EC2
      "spot" -> pure Spot
      e ->
        fromTextError $
          "Failure parsing CRType from value: '" <> e
            <> "'. Accepted values: ec2, spot"

instance ToText CRType where
  toText = \case
    EC2 -> "EC2"
    Spot -> "SPOT"

instance Hashable CRType

instance NFData CRType

instance ToByteString CRType

instance ToQuery CRType

instance ToHeader CRType

instance ToJSON CRType where
  toJSON = toJSONText

instance FromJSON CRType where
  parseJSON = parseJSONText "CRType"
