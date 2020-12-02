{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EndDateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EndDateType where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EndDateType
  = Limited
  | Unlimited
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

instance FromText EndDateType where
  parser =
    takeLowerText >>= \case
      "limited" -> pure Limited
      "unlimited" -> pure Unlimited
      e ->
        fromTextError $
          "Failure parsing EndDateType from value: '" <> e
            <> "'. Accepted values: limited, unlimited"

instance ToText EndDateType where
  toText = \case
    Limited -> "limited"
    Unlimited -> "unlimited"

instance Hashable EndDateType

instance NFData EndDateType

instance ToByteString EndDateType

instance ToQuery EndDateType

instance ToHeader EndDateType

instance FromXML EndDateType where
  parseXML = parseXMLText "EndDateType"
