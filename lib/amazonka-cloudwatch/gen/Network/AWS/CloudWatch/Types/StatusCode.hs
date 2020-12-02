{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.StatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.StatusCode where

import Network.AWS.Prelude

data StatusCode
  = Complete
  | InternalError
  | PartialData
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

instance FromText StatusCode where
  parser =
    takeLowerText >>= \case
      "complete" -> pure Complete
      "internalerror" -> pure InternalError
      "partialdata" -> pure PartialData
      e ->
        fromTextError $
          "Failure parsing StatusCode from value: '" <> e
            <> "'. Accepted values: complete, internalerror, partialdata"

instance ToText StatusCode where
  toText = \case
    Complete -> "Complete"
    InternalError -> "InternalError"
    PartialData -> "PartialData"

instance Hashable StatusCode

instance NFData StatusCode

instance ToByteString StatusCode

instance ToQuery StatusCode

instance ToHeader StatusCode

instance FromXML StatusCode where
  parseXML = parseXMLText "StatusCode"
