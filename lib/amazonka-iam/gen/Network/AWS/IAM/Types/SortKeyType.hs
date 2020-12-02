{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.SortKeyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.SortKeyType where

import Network.AWS.Prelude

data SortKeyType
  = LastAuthenticatedTimeAscending
  | LastAuthenticatedTimeDescending
  | ServiceNamespaceAscending
  | ServiceNamespaceDescending
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

instance FromText SortKeyType where
  parser =
    takeLowerText >>= \case
      "last_authenticated_time_ascending" -> pure LastAuthenticatedTimeAscending
      "last_authenticated_time_descending" -> pure LastAuthenticatedTimeDescending
      "service_namespace_ascending" -> pure ServiceNamespaceAscending
      "service_namespace_descending" -> pure ServiceNamespaceDescending
      e ->
        fromTextError $
          "Failure parsing SortKeyType from value: '" <> e
            <> "'. Accepted values: last_authenticated_time_ascending, last_authenticated_time_descending, service_namespace_ascending, service_namespace_descending"

instance ToText SortKeyType where
  toText = \case
    LastAuthenticatedTimeAscending -> "LAST_AUTHENTICATED_TIME_ASCENDING"
    LastAuthenticatedTimeDescending -> "LAST_AUTHENTICATED_TIME_DESCENDING"
    ServiceNamespaceAscending -> "SERVICE_NAMESPACE_ASCENDING"
    ServiceNamespaceDescending -> "SERVICE_NAMESPACE_DESCENDING"

instance Hashable SortKeyType

instance NFData SortKeyType

instance ToByteString SortKeyType

instance ToQuery SortKeyType

instance ToHeader SortKeyType
