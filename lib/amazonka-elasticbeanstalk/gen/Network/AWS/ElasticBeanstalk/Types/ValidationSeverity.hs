{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.ValidationSeverity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.ValidationSeverity where

import Network.AWS.Prelude

data ValidationSeverity
  = Error'
  | Warning
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

instance FromText ValidationSeverity where
  parser =
    takeLowerText >>= \case
      "error" -> pure Error'
      "warning" -> pure Warning
      e ->
        fromTextError $
          "Failure parsing ValidationSeverity from value: '" <> e
            <> "'. Accepted values: error, warning"

instance ToText ValidationSeverity where
  toText = \case
    Error' -> "error"
    Warning -> "warning"

instance Hashable ValidationSeverity

instance NFData ValidationSeverity

instance ToByteString ValidationSeverity

instance ToQuery ValidationSeverity

instance ToHeader ValidationSeverity

instance FromXML ValidationSeverity where
  parseXML = parseXMLText "ValidationSeverity"
