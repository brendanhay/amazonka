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
        DataCreatedAt,
        DataDATALOCATIONS3,
        DataIAMUser,
        DataLastUpdatedAt,
        DataName,
        DataStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

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
newtype DataSourceFilterVariable = DataSourceFilterVariable' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern DataCreatedAt :: DataSourceFilterVariable
pattern DataCreatedAt = DataSourceFilterVariable' "CreatedAt"

pattern DataDATALOCATIONS3 :: DataSourceFilterVariable
pattern DataDATALOCATIONS3 = DataSourceFilterVariable' "DataLocationS3"

pattern DataIAMUser :: DataSourceFilterVariable
pattern DataIAMUser = DataSourceFilterVariable' "IAMUser"

pattern DataLastUpdatedAt :: DataSourceFilterVariable
pattern DataLastUpdatedAt = DataSourceFilterVariable' "LastUpdatedAt"

pattern DataName :: DataSourceFilterVariable
pattern DataName = DataSourceFilterVariable' "Name"

pattern DataStatus :: DataSourceFilterVariable
pattern DataStatus = DataSourceFilterVariable' "Status"

{-# COMPLETE
  DataCreatedAt,
  DataDATALOCATIONS3,
  DataIAMUser,
  DataLastUpdatedAt,
  DataName,
  DataStatus,
  DataSourceFilterVariable'
  #-}
