{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstancePatchStateOperatorType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstancePatchStateOperatorType where

import Network.AWS.Prelude

data InstancePatchStateOperatorType
  = IPSOTEqual
  | IPSOTGreaterThan
  | IPSOTLessThan
  | IPSOTNotEqual
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

instance FromText InstancePatchStateOperatorType where
  parser =
    takeLowerText >>= \case
      "equal" -> pure IPSOTEqual
      "greaterthan" -> pure IPSOTGreaterThan
      "lessthan" -> pure IPSOTLessThan
      "notequal" -> pure IPSOTNotEqual
      e ->
        fromTextError $
          "Failure parsing InstancePatchStateOperatorType from value: '" <> e
            <> "'. Accepted values: equal, greaterthan, lessthan, notequal"

instance ToText InstancePatchStateOperatorType where
  toText = \case
    IPSOTEqual -> "Equal"
    IPSOTGreaterThan -> "GreaterThan"
    IPSOTLessThan -> "LessThan"
    IPSOTNotEqual -> "NotEqual"

instance Hashable InstancePatchStateOperatorType

instance NFData InstancePatchStateOperatorType

instance ToByteString InstancePatchStateOperatorType

instance ToQuery InstancePatchStateOperatorType

instance ToHeader InstancePatchStateOperatorType

instance ToJSON InstancePatchStateOperatorType where
  toJSON = toJSONText
