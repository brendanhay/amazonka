{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.AttachmentHashType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.AttachmentHashType where

import Network.AWS.Prelude

data AttachmentHashType = SHA256
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

instance FromText AttachmentHashType where
  parser =
    takeLowerText >>= \case
      "sha256" -> pure SHA256
      e ->
        fromTextError $
          "Failure parsing AttachmentHashType from value: '" <> e
            <> "'. Accepted values: sha256"

instance ToText AttachmentHashType where
  toText = \case
    SHA256 -> "Sha256"

instance Hashable AttachmentHashType

instance NFData AttachmentHashType

instance ToByteString AttachmentHashType

instance ToQuery AttachmentHashType

instance ToHeader AttachmentHashType

instance FromJSON AttachmentHashType where
  parseJSON = parseJSONText "AttachmentHashType"
