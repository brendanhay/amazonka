{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetTypeEnum where

import Network.AWS.Prelude

data TargetTypeEnum
  = IP
  | Instance
  | Lambda
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

instance FromText TargetTypeEnum where
  parser =
    takeLowerText >>= \case
      "ip" -> pure IP
      "instance" -> pure Instance
      "lambda" -> pure Lambda
      e ->
        fromTextError $
          "Failure parsing TargetTypeEnum from value: '" <> e
            <> "'. Accepted values: ip, instance, lambda"

instance ToText TargetTypeEnum where
  toText = \case
    IP -> "ip"
    Instance -> "instance"
    Lambda -> "lambda"

instance Hashable TargetTypeEnum

instance NFData TargetTypeEnum

instance ToByteString TargetTypeEnum

instance ToQuery TargetTypeEnum

instance ToHeader TargetTypeEnum

instance FromXML TargetTypeEnum where
  parseXML = parseXMLText "TargetTypeEnum"
