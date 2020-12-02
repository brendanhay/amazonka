{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.TableRestoreStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.TableRestoreStatusType where

import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

data TableRestoreStatusType
  = TRSTCanceled
  | TRSTFailed
  | TRSTInProgress
  | TRSTPending
  | TRSTSucceeded
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

instance FromText TableRestoreStatusType where
  parser =
    takeLowerText >>= \case
      "canceled" -> pure TRSTCanceled
      "failed" -> pure TRSTFailed
      "in_progress" -> pure TRSTInProgress
      "pending" -> pure TRSTPending
      "succeeded" -> pure TRSTSucceeded
      e ->
        fromTextError $
          "Failure parsing TableRestoreStatusType from value: '" <> e
            <> "'. Accepted values: canceled, failed, in_progress, pending, succeeded"

instance ToText TableRestoreStatusType where
  toText = \case
    TRSTCanceled -> "CANCELED"
    TRSTFailed -> "FAILED"
    TRSTInProgress -> "IN_PROGRESS"
    TRSTPending -> "PENDING"
    TRSTSucceeded -> "SUCCEEDED"

instance Hashable TableRestoreStatusType

instance NFData TableRestoreStatusType

instance ToByteString TableRestoreStatusType

instance ToQuery TableRestoreStatusType

instance ToHeader TableRestoreStatusType

instance FromXML TableRestoreStatusType where
  parseXML = parseXMLText "TableRestoreStatusType"
