{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.TemplateStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.TemplateStage where

import Network.AWS.Prelude

data TemplateStage
  = Original
  | Processed
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

instance FromText TemplateStage where
  parser =
    takeLowerText >>= \case
      "original" -> pure Original
      "processed" -> pure Processed
      e ->
        fromTextError $
          "Failure parsing TemplateStage from value: '" <> e
            <> "'. Accepted values: original, processed"

instance ToText TemplateStage where
  toText = \case
    Original -> "Original"
    Processed -> "Processed"

instance Hashable TemplateStage

instance NFData TemplateStage

instance ToByteString TemplateStage

instance ToQuery TemplateStage

instance ToHeader TemplateStage

instance FromXML TemplateStage where
  parseXML = parseXMLText "TemplateStage"
