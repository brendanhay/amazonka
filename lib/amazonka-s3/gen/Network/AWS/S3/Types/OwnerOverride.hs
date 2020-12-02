{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.OwnerOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.OwnerOverride where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data OwnerOverride = Destination
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

instance FromText OwnerOverride where
  parser =
    takeLowerText >>= \case
      "destination" -> pure Destination
      e ->
        fromTextError $
          "Failure parsing OwnerOverride from value: '" <> e
            <> "'. Accepted values: destination"

instance ToText OwnerOverride where
  toText = \case
    Destination -> "Destination"

instance Hashable OwnerOverride

instance NFData OwnerOverride

instance ToByteString OwnerOverride

instance ToQuery OwnerOverride

instance ToHeader OwnerOverride

instance FromXML OwnerOverride where
  parseXML = parseXMLText "OwnerOverride"

instance ToXML OwnerOverride where
  toXML = toXMLText
