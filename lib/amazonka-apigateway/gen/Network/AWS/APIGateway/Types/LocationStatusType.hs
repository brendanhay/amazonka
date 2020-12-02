{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.LocationStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.LocationStatusType where

import Network.AWS.Prelude

data LocationStatusType
  = Documented
  | Undocumented
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

instance FromText LocationStatusType where
  parser =
    takeLowerText >>= \case
      "documented" -> pure Documented
      "undocumented" -> pure Undocumented
      e ->
        fromTextError $
          "Failure parsing LocationStatusType from value: '" <> e
            <> "'. Accepted values: documented, undocumented"

instance ToText LocationStatusType where
  toText = \case
    Documented -> "DOCUMENTED"
    Undocumented -> "UNDOCUMENTED"

instance Hashable LocationStatusType

instance NFData LocationStatusType

instance ToByteString LocationStatusType

instance ToQuery LocationStatusType

instance ToHeader LocationStatusType

instance ToJSON LocationStatusType where
  toJSON = toJSONText
