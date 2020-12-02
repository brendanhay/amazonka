{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.JobTemplateListBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobTemplateListBy where

import Network.AWS.Prelude

-- | Optional. When you request a list of job templates, you can choose to list them alphabetically by NAME or chronologically by CREATION_DATE. If you don't specify, the service will list them by name.
data JobTemplateListBy
  = JTLBCreationDate
  | JTLBName
  | JTLBSystem
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

instance FromText JobTemplateListBy where
  parser =
    takeLowerText >>= \case
      "creation_date" -> pure JTLBCreationDate
      "name" -> pure JTLBName
      "system" -> pure JTLBSystem
      e ->
        fromTextError $
          "Failure parsing JobTemplateListBy from value: '" <> e
            <> "'. Accepted values: creation_date, name, system"

instance ToText JobTemplateListBy where
  toText = \case
    JTLBCreationDate -> "CREATION_DATE"
    JTLBName -> "NAME"
    JTLBSystem -> "SYSTEM"

instance Hashable JobTemplateListBy

instance NFData JobTemplateListBy

instance ToByteString JobTemplateListBy

instance ToQuery JobTemplateListBy

instance ToHeader JobTemplateListBy

instance ToJSON JobTemplateListBy where
  toJSON = toJSONText
