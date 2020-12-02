{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnlimitedSupportedInstanceFamily where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data UnlimitedSupportedInstanceFamily
  = T2
  | T3
  | T3a
  | T4g
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

instance FromText UnlimitedSupportedInstanceFamily where
  parser =
    takeLowerText >>= \case
      "t2" -> pure T2
      "t3" -> pure T3
      "t3a" -> pure T3a
      "t4g" -> pure T4g
      e ->
        fromTextError $
          "Failure parsing UnlimitedSupportedInstanceFamily from value: '" <> e
            <> "'. Accepted values: t2, t3, t3a, t4g"

instance ToText UnlimitedSupportedInstanceFamily where
  toText = \case
    T2 -> "t2"
    T3 -> "t3"
    T3a -> "t3a"
    T4g -> "t4g"

instance Hashable UnlimitedSupportedInstanceFamily

instance NFData UnlimitedSupportedInstanceFamily

instance ToByteString UnlimitedSupportedInstanceFamily

instance ToQuery UnlimitedSupportedInstanceFamily

instance ToHeader UnlimitedSupportedInstanceFamily

instance FromXML UnlimitedSupportedInstanceFamily where
  parseXML = parseXMLText "UnlimitedSupportedInstanceFamily"
