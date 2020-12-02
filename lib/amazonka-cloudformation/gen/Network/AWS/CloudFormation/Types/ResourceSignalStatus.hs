{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceSignalStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceSignalStatus where

import Network.AWS.Prelude

data ResourceSignalStatus
  = Failure
  | Success
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

instance FromText ResourceSignalStatus where
  parser =
    takeLowerText >>= \case
      "failure" -> pure Failure
      "success" -> pure Success
      e ->
        fromTextError $
          "Failure parsing ResourceSignalStatus from value: '" <> e
            <> "'. Accepted values: failure, success"

instance ToText ResourceSignalStatus where
  toText = \case
    Failure -> "FAILURE"
    Success -> "SUCCESS"

instance Hashable ResourceSignalStatus

instance NFData ResourceSignalStatus

instance ToByteString ResourceSignalStatus

instance ToQuery ResourceSignalStatus

instance ToHeader ResourceSignalStatus
