{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.MessageSystemAttributeNameForSends
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.MessageSystemAttributeNameForSends where

import Network.AWS.Prelude

data MessageSystemAttributeNameForSends = AWSTraceHeader
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

instance FromText MessageSystemAttributeNameForSends where
  parser =
    takeLowerText >>= \case
      "awstraceheader" -> pure AWSTraceHeader
      e ->
        fromTextError $
          "Failure parsing MessageSystemAttributeNameForSends from value: '" <> e
            <> "'. Accepted values: awstraceheader"

instance ToText MessageSystemAttributeNameForSends where
  toText = \case
    AWSTraceHeader -> "AWSTraceHeader"

instance Hashable MessageSystemAttributeNameForSends

instance NFData MessageSystemAttributeNameForSends

instance ToByteString MessageSystemAttributeNameForSends

instance ToQuery MessageSystemAttributeNameForSends

instance ToHeader MessageSystemAttributeNameForSends
