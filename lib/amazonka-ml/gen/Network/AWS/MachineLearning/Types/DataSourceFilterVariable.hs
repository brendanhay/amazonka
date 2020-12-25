{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MachineLearning.Types.DataSourceFilterVariable
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.DataSourceFilterVariable
  ( DataSourceFilterVariable
      ( DataSourceFilterVariable',
        DataSourceFilterVariableDataCreatedAt,
        DataSourceFilterVariableDataLastUpdatedAt,
        DataSourceFilterVariableDataStatus,
        DataSourceFilterVariableDataName,
        DataSourceFilterVariableDataDATALOCATIONS3,
        DataSourceFilterVariableDataIAMUser,
        fromDataSourceFilterVariable
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | A list of the variables to use in searching or filtering @DataSource@ .
--
--
--     * @CreatedAt@ - Sets the search criteria to @DataSource@ creation date.
--
--     * @Status@ - Sets the search criteria to @DataSource@ status.
--
--     * @Name@ - Sets the search criteria to the contents of @DataSource@ ____ @Name@ .
--
--     * @DataUri@ - Sets the search criteria to the URI of data files used to create the @DataSource@ . The URI can identify either a file or an Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
--     * @IAMUser@ - Sets the search criteria to the user account that invoked the @DataSource@ creation.
newtype DataSourceFilterVariable = DataSourceFilterVariable'
  { fromDataSourceFilterVariable ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern DataSourceFilterVariableDataCreatedAt :: DataSourceFilterVariable
pattern DataSourceFilterVariableDataCreatedAt = DataSourceFilterVariable' "CreatedAt"

pattern DataSourceFilterVariableDataLastUpdatedAt :: DataSourceFilterVariable
pattern DataSourceFilterVariableDataLastUpdatedAt = DataSourceFilterVariable' "LastUpdatedAt"

pattern DataSourceFilterVariableDataStatus :: DataSourceFilterVariable
pattern DataSourceFilterVariableDataStatus = DataSourceFilterVariable' "Status"

pattern DataSourceFilterVariableDataName :: DataSourceFilterVariable
pattern DataSourceFilterVariableDataName = DataSourceFilterVariable' "Name"

pattern DataSourceFilterVariableDataDATALOCATIONS3 :: DataSourceFilterVariable
pattern DataSourceFilterVariableDataDATALOCATIONS3 = DataSourceFilterVariable' "DataLocationS3"

pattern DataSourceFilterVariableDataIAMUser :: DataSourceFilterVariable
pattern DataSourceFilterVariableDataIAMUser = DataSourceFilterVariable' "IAMUser"

{-# COMPLETE
  DataSourceFilterVariableDataCreatedAt,
  DataSourceFilterVariableDataLastUpdatedAt,
  DataSourceFilterVariableDataStatus,
  DataSourceFilterVariableDataName,
  DataSourceFilterVariableDataDATALOCATIONS3,
  DataSourceFilterVariableDataIAMUser,
  DataSourceFilterVariable'
  #-}
