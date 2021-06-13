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
-- Module      : Network.AWS.MachineLearning.Types.DataSourceFilterVariable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MachineLearning.Types.DataSourceFilterVariable
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | A list of the variables to use in searching or filtering @DataSource@.
--
-- -   @CreatedAt@ - Sets the search criteria to @DataSource@ creation
--     date.
-- -   @Status@ - Sets the search criteria to @DataSource@ status.
-- -   @Name@ - Sets the search criteria to the contents of @DataSource@
--     ____ @Name@.
-- -   @DataUri@ - Sets the search criteria to the URI of data files used
--     to create the @DataSource@. The URI can identify either a file or an
--     Amazon Simple Storage Service (Amazon S3) bucket or directory.
-- -   @IAMUser@ - Sets the search criteria to the user account that
--     invoked the @DataSource@ creation.
--
-- Note
--
-- The variable names should match the variable names in the @DataSource@.
newtype DataSourceFilterVariable = DataSourceFilterVariable'
  { fromDataSourceFilterVariable ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
