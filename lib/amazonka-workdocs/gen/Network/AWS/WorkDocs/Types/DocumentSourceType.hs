{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.DocumentSourceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.DocumentSourceType where

import Network.AWS.Prelude

data DocumentSourceType
  = Original
  | WithComments
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

instance FromText DocumentSourceType where
  parser =
    takeLowerText >>= \case
      "original" -> pure Original
      "with_comments" -> pure WithComments
      e ->
        fromTextError $
          "Failure parsing DocumentSourceType from value: '" <> e
            <> "'. Accepted values: original, with_comments"

instance ToText DocumentSourceType where
  toText = \case
    Original -> "ORIGINAL"
    WithComments -> "WITH_COMMENTS"

instance Hashable DocumentSourceType

instance NFData DocumentSourceType

instance ToByteString DocumentSourceType

instance ToQuery DocumentSourceType

instance ToHeader DocumentSourceType

instance FromJSON DocumentSourceType where
  parseJSON = parseJSONText "DocumentSourceType"
