{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationFilterName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationFilterName where

import Network.AWS.Prelude

data OperationFilterName
  = OFNNamespaceId
  | OFNServiceId
  | OFNStatus
  | OFNType
  | OFNUpdateDate
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

instance FromText OperationFilterName where
  parser =
    takeLowerText >>= \case
      "namespace_id" -> pure OFNNamespaceId
      "service_id" -> pure OFNServiceId
      "status" -> pure OFNStatus
      "type" -> pure OFNType
      "update_date" -> pure OFNUpdateDate
      e ->
        fromTextError $
          "Failure parsing OperationFilterName from value: '" <> e
            <> "'. Accepted values: namespace_id, service_id, status, type, update_date"

instance ToText OperationFilterName where
  toText = \case
    OFNNamespaceId -> "NAMESPACE_ID"
    OFNServiceId -> "SERVICE_ID"
    OFNStatus -> "STATUS"
    OFNType -> "TYPE"
    OFNUpdateDate -> "UPDATE_DATE"

instance Hashable OperationFilterName

instance NFData OperationFilterName

instance ToByteString OperationFilterName

instance ToQuery OperationFilterName

instance ToHeader OperationFilterName

instance ToJSON OperationFilterName where
  toJSON = toJSONText
