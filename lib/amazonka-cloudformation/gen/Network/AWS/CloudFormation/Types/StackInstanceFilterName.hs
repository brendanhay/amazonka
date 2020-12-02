{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackInstanceFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackInstanceFilterName where

import Network.AWS.Prelude

data StackInstanceFilterName = DetailedStatus
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

instance FromText StackInstanceFilterName where
  parser =
    takeLowerText >>= \case
      "detailed_status" -> pure DetailedStatus
      e ->
        fromTextError $
          "Failure parsing StackInstanceFilterName from value: '" <> e
            <> "'. Accepted values: detailed_status"

instance ToText StackInstanceFilterName where
  toText = \case
    DetailedStatus -> "DETAILED_STATUS"

instance Hashable StackInstanceFilterName

instance NFData StackInstanceFilterName

instance ToByteString StackInstanceFilterName

instance ToQuery StackInstanceFilterName

instance ToHeader StackInstanceFilterName
