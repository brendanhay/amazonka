{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ChangeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ChangeType where

import Network.AWS.Prelude

data ChangeType = CTResource
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

instance FromText ChangeType where
  parser =
    takeLowerText >>= \case
      "resource" -> pure CTResource
      e ->
        fromTextError $
          "Failure parsing ChangeType from value: '" <> e
            <> "'. Accepted values: resource"

instance ToText ChangeType where
  toText = \case
    CTResource -> "Resource"

instance Hashable ChangeType

instance NFData ChangeType

instance ToByteString ChangeType

instance ToQuery ChangeType

instance ToHeader ChangeType

instance FromXML ChangeType where
  parseXML = parseXMLText "ChangeType"
