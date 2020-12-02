{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode where

import Network.AWS.Prelude

data BusinessReportFailureCode
  = AccessDenied
  | InternalFailure
  | NoSuchBucket
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

instance FromText BusinessReportFailureCode where
  parser =
    takeLowerText >>= \case
      "access_denied" -> pure AccessDenied
      "internal_failure" -> pure InternalFailure
      "no_such_bucket" -> pure NoSuchBucket
      e ->
        fromTextError $
          "Failure parsing BusinessReportFailureCode from value: '" <> e
            <> "'. Accepted values: access_denied, internal_failure, no_such_bucket"

instance ToText BusinessReportFailureCode where
  toText = \case
    AccessDenied -> "ACCESS_DENIED"
    InternalFailure -> "INTERNAL_FAILURE"
    NoSuchBucket -> "NO_SUCH_BUCKET"

instance Hashable BusinessReportFailureCode

instance NFData BusinessReportFailureCode

instance ToByteString BusinessReportFailureCode

instance ToQuery BusinessReportFailureCode

instance ToHeader BusinessReportFailureCode

instance FromJSON BusinessReportFailureCode where
  parseJSON = parseJSONText "BusinessReportFailureCode"
