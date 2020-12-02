{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.DataSourceFilterVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.DataSourceFilterVariable where

import Network.AWS.Prelude

-- | A list of the variables to use in searching or filtering @DataSource@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @DataSource@ creation date.    * @Status@ - Sets the search criteria to @DataSource@ status.    * @Name@ - Sets the search criteria to the contents of @DataSource@ ____ @Name@ .    * @DataUri@ - Sets the search criteria to the URI of data files used to create the @DataSource@ . The URI can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.    * @IAMUser@ - Sets the search criteria to the user account that invoked the @DataSource@ creation.
data DataSourceFilterVariable
  = DataCreatedAt
  | DataDATALOCATIONS3
  | DataIAMUser
  | DataLastUpdatedAt
  | DataName
  | DataStatus
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

instance FromText DataSourceFilterVariable where
  parser =
    takeLowerText >>= \case
      "createdat" -> pure DataCreatedAt
      "datalocations3" -> pure DataDATALOCATIONS3
      "iamuser" -> pure DataIAMUser
      "lastupdatedat" -> pure DataLastUpdatedAt
      "name" -> pure DataName
      "status" -> pure DataStatus
      e ->
        fromTextError $
          "Failure parsing DataSourceFilterVariable from value: '" <> e
            <> "'. Accepted values: createdat, datalocations3, iamuser, lastupdatedat, name, status"

instance ToText DataSourceFilterVariable where
  toText = \case
    DataCreatedAt -> "CreatedAt"
    DataDATALOCATIONS3 -> "DataLocationS3"
    DataIAMUser -> "IAMUser"
    DataLastUpdatedAt -> "LastUpdatedAt"
    DataName -> "Name"
    DataStatus -> "Status"

instance Hashable DataSourceFilterVariable

instance NFData DataSourceFilterVariable

instance ToByteString DataSourceFilterVariable

instance ToQuery DataSourceFilterVariable

instance ToHeader DataSourceFilterVariable

instance ToJSON DataSourceFilterVariable where
  toJSON = toJSONText
