{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.RestoreRequestType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.RestoreRequestType where

import Network.AWS.Prelude
import Network.AWS.S3.Internal

data RestoreRequestType = Select
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

instance FromText RestoreRequestType where
  parser =
    takeLowerText >>= \case
      "select" -> pure Select
      e ->
        fromTextError $
          "Failure parsing RestoreRequestType from value: '" <> e
            <> "'. Accepted values: select"

instance ToText RestoreRequestType where
  toText = \case
    Select -> "SELECT"

instance Hashable RestoreRequestType

instance NFData RestoreRequestType

instance ToByteString RestoreRequestType

instance ToQuery RestoreRequestType

instance ToHeader RestoreRequestType

instance ToXML RestoreRequestType where
  toXML = toXMLText
