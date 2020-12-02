{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ResettableElementName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ResettableElementName where

import Network.AWS.Prelude
import Network.AWS.Route53.Internal

data ResettableElementName
  = ChildHealthChecks
  | FullyQualifiedDomainName
  | Regions
  | ResourcePath
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

instance FromText ResettableElementName where
  parser =
    takeLowerText >>= \case
      "childhealthchecks" -> pure ChildHealthChecks
      "fullyqualifieddomainname" -> pure FullyQualifiedDomainName
      "regions" -> pure Regions
      "resourcepath" -> pure ResourcePath
      e ->
        fromTextError $
          "Failure parsing ResettableElementName from value: '" <> e
            <> "'. Accepted values: childhealthchecks, fullyqualifieddomainname, regions, resourcepath"

instance ToText ResettableElementName where
  toText = \case
    ChildHealthChecks -> "ChildHealthChecks"
    FullyQualifiedDomainName -> "FullyQualifiedDomainName"
    Regions -> "Regions"
    ResourcePath -> "ResourcePath"

instance Hashable ResettableElementName

instance NFData ResettableElementName

instance ToByteString ResettableElementName

instance ToQuery ResettableElementName

instance ToHeader ResettableElementName

instance ToXML ResettableElementName where
  toXML = toXMLText
