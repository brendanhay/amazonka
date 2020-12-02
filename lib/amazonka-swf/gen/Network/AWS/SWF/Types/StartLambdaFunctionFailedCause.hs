{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.StartLambdaFunctionFailedCause
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.StartLambdaFunctionFailedCause where

import Network.AWS.Prelude

data StartLambdaFunctionFailedCause = AssumeRoleFailed
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

instance FromText StartLambdaFunctionFailedCause where
  parser =
    takeLowerText >>= \case
      "assume_role_failed" -> pure AssumeRoleFailed
      e ->
        fromTextError $
          "Failure parsing StartLambdaFunctionFailedCause from value: '" <> e
            <> "'. Accepted values: assume_role_failed"

instance ToText StartLambdaFunctionFailedCause where
  toText = \case
    AssumeRoleFailed -> "ASSUME_ROLE_FAILED"

instance Hashable StartLambdaFunctionFailedCause

instance NFData StartLambdaFunctionFailedCause

instance ToByteString StartLambdaFunctionFailedCause

instance ToQuery StartLambdaFunctionFailedCause

instance ToHeader StartLambdaFunctionFailedCause

instance FromJSON StartLambdaFunctionFailedCause where
  parseJSON = parseJSONText "StartLambdaFunctionFailedCause"
