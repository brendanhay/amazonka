{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.StopType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.StopType where

import Network.AWS.Prelude

data StopType
  = STCancel
  | STComplete
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

instance FromText StopType where
  parser =
    takeLowerText >>= \case
      "cancel" -> pure STCancel
      "complete" -> pure STComplete
      e ->
        fromTextError $
          "Failure parsing StopType from value: '" <> e
            <> "'. Accepted values: cancel, complete"

instance ToText StopType where
  toText = \case
    STCancel -> "Cancel"
    STComplete -> "Complete"

instance Hashable StopType

instance NFData StopType

instance ToByteString StopType

instance ToQuery StopType

instance ToHeader StopType

instance ToJSON StopType where
  toJSON = toJSONText
