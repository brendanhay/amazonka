{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.PortfolioShareType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.PortfolioShareType where

import Network.AWS.Prelude

data PortfolioShareType
  = AWSOrganizations
  | AWSServicecatalog
  | Imported
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

instance FromText PortfolioShareType where
  parser =
    takeLowerText >>= \case
      "aws_organizations" -> pure AWSOrganizations
      "aws_servicecatalog" -> pure AWSServicecatalog
      "imported" -> pure Imported
      e ->
        fromTextError $
          "Failure parsing PortfolioShareType from value: '" <> e
            <> "'. Accepted values: aws_organizations, aws_servicecatalog, imported"

instance ToText PortfolioShareType where
  toText = \case
    AWSOrganizations -> "AWS_ORGANIZATIONS"
    AWSServicecatalog -> "AWS_SERVICECATALOG"
    Imported -> "IMPORTED"

instance Hashable PortfolioShareType

instance NFData PortfolioShareType

instance ToByteString PortfolioShareType

instance ToQuery PortfolioShareType

instance ToHeader PortfolioShareType

instance ToJSON PortfolioShareType where
  toJSON = toJSONText
