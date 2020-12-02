{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.DimensionValueSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.DimensionValueSource where

import Network.AWS.Prelude

data DimensionValueSource
  = EmailHeader
  | LinkTag
  | MessageTag
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

instance FromText DimensionValueSource where
  parser =
    takeLowerText >>= \case
      "emailheader" -> pure EmailHeader
      "linktag" -> pure LinkTag
      "messagetag" -> pure MessageTag
      e ->
        fromTextError $
          "Failure parsing DimensionValueSource from value: '" <> e
            <> "'. Accepted values: emailheader, linktag, messagetag"

instance ToText DimensionValueSource where
  toText = \case
    EmailHeader -> "emailHeader"
    LinkTag -> "linkTag"
    MessageTag -> "messageTag"

instance Hashable DimensionValueSource

instance NFData DimensionValueSource

instance ToByteString DimensionValueSource

instance ToQuery DimensionValueSource

instance ToHeader DimensionValueSource

instance FromXML DimensionValueSource where
  parseXML = parseXMLText "DimensionValueSource"
