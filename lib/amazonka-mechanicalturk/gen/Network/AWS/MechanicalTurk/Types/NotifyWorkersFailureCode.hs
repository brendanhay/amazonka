{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.NotifyWorkersFailureCode where

import Network.AWS.Prelude

data NotifyWorkersFailureCode
  = HardFailure
  | SoftFailure
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

instance FromText NotifyWorkersFailureCode where
  parser =
    takeLowerText >>= \case
      "hardfailure" -> pure HardFailure
      "softfailure" -> pure SoftFailure
      e ->
        fromTextError $
          "Failure parsing NotifyWorkersFailureCode from value: '" <> e
            <> "'. Accepted values: hardfailure, softfailure"

instance ToText NotifyWorkersFailureCode where
  toText = \case
    HardFailure -> "HardFailure"
    SoftFailure -> "SoftFailure"

instance Hashable NotifyWorkersFailureCode

instance NFData NotifyWorkersFailureCode

instance ToByteString NotifyWorkersFailureCode

instance ToQuery NotifyWorkersFailureCode

instance ToHeader NotifyWorkersFailureCode

instance FromJSON NotifyWorkersFailureCode where
  parseJSON = parseJSONText "NotifyWorkersFailureCode"
