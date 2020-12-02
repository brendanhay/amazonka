{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSEncryptionSupport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSEncryptionSupport where

import Network.AWS.EC2.Internal
import Network.AWS.Prelude

data EBSEncryptionSupport
  = Supported
  | Unsupported
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

instance FromText EBSEncryptionSupport where
  parser =
    takeLowerText >>= \case
      "supported" -> pure Supported
      "unsupported" -> pure Unsupported
      e ->
        fromTextError $
          "Failure parsing EBSEncryptionSupport from value: '" <> e
            <> "'. Accepted values: supported, unsupported"

instance ToText EBSEncryptionSupport where
  toText = \case
    Supported -> "supported"
    Unsupported -> "unsupported"

instance Hashable EBSEncryptionSupport

instance NFData EBSEncryptionSupport

instance ToByteString EBSEncryptionSupport

instance ToQuery EBSEncryptionSupport

instance ToHeader EBSEncryptionSupport

instance FromXML EBSEncryptionSupport where
  parseXML = parseXMLText "EBSEncryptionSupport"
