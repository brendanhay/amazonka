{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ParameterApplyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ParameterApplyType where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data ParameterApplyType
  = Dynamic
  | Static
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

instance FromText ParameterApplyType where
  parser =
    takeLowerText >>= \case
      "dynamic" -> pure Dynamic
      "static" -> pure Static
      e ->
        fromTextError $
          "Failure parsing ParameterApplyType from value: '" <> e
            <> "'. Accepted values: dynamic, static"

instance ToText ParameterApplyType where
  toText = \case
    Dynamic -> "dynamic"
    Static -> "static"

instance Hashable ParameterApplyType

instance NFData ParameterApplyType

instance ToByteString ParameterApplyType

instance ToQuery ParameterApplyType

instance ToHeader ParameterApplyType

instance FromXML ParameterApplyType where
  parseXML = parseXMLText "ParameterApplyType"
