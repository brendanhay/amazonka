{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ArtifactPackaging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ArtifactPackaging where

import Network.AWS.Prelude

data ArtifactPackaging
  = None
  | Zip
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

instance FromText ArtifactPackaging where
  parser =
    takeLowerText >>= \case
      "none" -> pure None
      "zip" -> pure Zip
      e ->
        fromTextError $
          "Failure parsing ArtifactPackaging from value: '" <> e
            <> "'. Accepted values: none, zip"

instance ToText ArtifactPackaging where
  toText = \case
    None -> "NONE"
    Zip -> "ZIP"

instance Hashable ArtifactPackaging

instance NFData ArtifactPackaging

instance ToByteString ArtifactPackaging

instance ToQuery ArtifactPackaging

instance ToHeader ArtifactPackaging

instance ToJSON ArtifactPackaging where
  toJSON = toJSONText

instance FromJSON ArtifactPackaging where
  parseJSON = parseJSONText "ArtifactPackaging"
