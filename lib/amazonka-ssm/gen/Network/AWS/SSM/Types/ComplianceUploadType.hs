{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ComplianceUploadType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceUploadType where

import Network.AWS.Prelude

data ComplianceUploadType
  = Complete
  | Partial
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

instance FromText ComplianceUploadType where
  parser =
    takeLowerText >>= \case
      "complete" -> pure Complete
      "partial" -> pure Partial
      e ->
        fromTextError $
          "Failure parsing ComplianceUploadType from value: '" <> e
            <> "'. Accepted values: complete, partial"

instance ToText ComplianceUploadType where
  toText = \case
    Complete -> "COMPLETE"
    Partial -> "PARTIAL"

instance Hashable ComplianceUploadType

instance NFData ComplianceUploadType

instance ToByteString ComplianceUploadType

instance ToQuery ComplianceUploadType

instance ToHeader ComplianceUploadType

instance ToJSON ComplianceUploadType where
  toJSON = toJSONText
