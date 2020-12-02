{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BounceType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BounceType where

import Network.AWS.Prelude

data BounceType
  = ContentRejected
  | DoesNotExist
  | ExceededQuota
  | MessageTooLarge
  | TemporaryFailure
  | Undefined
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

instance FromText BounceType where
  parser =
    takeLowerText >>= \case
      "contentrejected" -> pure ContentRejected
      "doesnotexist" -> pure DoesNotExist
      "exceededquota" -> pure ExceededQuota
      "messagetoolarge" -> pure MessageTooLarge
      "temporaryfailure" -> pure TemporaryFailure
      "undefined" -> pure Undefined
      e ->
        fromTextError $
          "Failure parsing BounceType from value: '" <> e
            <> "'. Accepted values: contentrejected, doesnotexist, exceededquota, messagetoolarge, temporaryfailure, undefined"

instance ToText BounceType where
  toText = \case
    ContentRejected -> "ContentRejected"
    DoesNotExist -> "DoesNotExist"
    ExceededQuota -> "ExceededQuota"
    MessageTooLarge -> "MessageTooLarge"
    TemporaryFailure -> "TemporaryFailure"
    Undefined -> "Undefined"

instance Hashable BounceType

instance NFData BounceType

instance ToByteString BounceType

instance ToQuery BounceType

instance ToHeader BounceType
