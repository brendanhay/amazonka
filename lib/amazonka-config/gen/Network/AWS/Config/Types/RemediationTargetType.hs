{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationTargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationTargetType where

import Network.AWS.Prelude

data RemediationTargetType = SsmDocument
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

instance FromText RemediationTargetType where
  parser =
    takeLowerText >>= \case
      "ssm_document" -> pure SsmDocument
      e ->
        fromTextError $
          "Failure parsing RemediationTargetType from value: '" <> e
            <> "'. Accepted values: ssm_document"

instance ToText RemediationTargetType where
  toText = \case
    SsmDocument -> "SSM_DOCUMENT"

instance Hashable RemediationTargetType

instance NFData RemediationTargetType

instance ToByteString RemediationTargetType

instance ToQuery RemediationTargetType

instance ToHeader RemediationTargetType

instance ToJSON RemediationTargetType where
  toJSON = toJSONText

instance FromJSON RemediationTargetType where
  parseJSON = parseJSONText "RemediationTargetType"
