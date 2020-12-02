{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentVersionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentVersionStatus where

import Network.AWS.Prelude

data DocumentVersionStatus = DVSActive
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

instance FromText DocumentVersionStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure DVSActive
      e ->
        fromTextError $
          "Failure parsing DocumentVersionStatus from value: '" <> e
            <> "'. Accepted values: active"

instance ToText DocumentVersionStatus where
  toText = \case
    DVSActive -> "ACTIVE"

instance Hashable DocumentVersionStatus

instance NFData DocumentVersionStatus

instance ToByteString DocumentVersionStatus

instance ToQuery DocumentVersionStatus

instance ToHeader DocumentVersionStatus

instance ToJSON DocumentVersionStatus where
  toJSON = toJSONText
