{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MachineLearning.Types.DataSourceFilterVariable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MachineLearning.Types.DataSourceFilterVariable
  ( DataSourceFilterVariable
      ( ..,
        DataSourceFilterVariable_CreatedAt,
        DataSourceFilterVariable_DataLocationS3,
        DataSourceFilterVariable_IAMUser,
        DataSourceFilterVariable_LastUpdatedAt,
        DataSourceFilterVariable_Name,
        DataSourceFilterVariable_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of the variables to use in searching or filtering @DataSource@.
--
-- -   @CreatedAt@ - Sets the search criteria to @DataSource@ creation
--     date.
--
-- -   @Status@ - Sets the search criteria to @DataSource@ status.
--
-- -   @Name@ - Sets the search criteria to the contents of @DataSource@
--     @Name@.
--
-- -   @DataUri@ - Sets the search criteria to the URI of data files used
--     to create the @DataSource@. The URI can identify either a file or an
--     Amazon Simple Storage Service (Amazon S3) bucket or directory.
--
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @DataSource@ creation.
--
-- __Note:__ The variable names should match the variable names in the
-- @DataSource@.
newtype DataSourceFilterVariable = DataSourceFilterVariable'
  { fromDataSourceFilterVariable ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern DataSourceFilterVariable_CreatedAt :: DataSourceFilterVariable
pattern DataSourceFilterVariable_CreatedAt = DataSourceFilterVariable' "CreatedAt"

pattern DataSourceFilterVariable_DataLocationS3 :: DataSourceFilterVariable
pattern DataSourceFilterVariable_DataLocationS3 = DataSourceFilterVariable' "DataLocationS3"

pattern DataSourceFilterVariable_IAMUser :: DataSourceFilterVariable
pattern DataSourceFilterVariable_IAMUser = DataSourceFilterVariable' "IAMUser"

pattern DataSourceFilterVariable_LastUpdatedAt :: DataSourceFilterVariable
pattern DataSourceFilterVariable_LastUpdatedAt = DataSourceFilterVariable' "LastUpdatedAt"

pattern DataSourceFilterVariable_Name :: DataSourceFilterVariable
pattern DataSourceFilterVariable_Name = DataSourceFilterVariable' "Name"

pattern DataSourceFilterVariable_Status :: DataSourceFilterVariable
pattern DataSourceFilterVariable_Status = DataSourceFilterVariable' "Status"

{-# COMPLETE
  DataSourceFilterVariable_CreatedAt,
  DataSourceFilterVariable_DataLocationS3,
  DataSourceFilterVariable_IAMUser,
  DataSourceFilterVariable_LastUpdatedAt,
  DataSourceFilterVariable_Name,
  DataSourceFilterVariable_Status,
  DataSourceFilterVariable'
  #-}
