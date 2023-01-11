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
-- Module      : Amazonka.DataExchange.Types.JobErrorLimitName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.JobErrorLimitName
  ( JobErrorLimitName
      ( ..,
        JobErrorLimitName_AWS_Lake_Formation_data_permission_assets_per_revision,
        JobErrorLimitName_Amazon_Redshift_datashare_assets_per_revision,
        JobErrorLimitName_Amazon_S3_data_access_assets_per_revision,
        JobErrorLimitName_Asset_size_in_GB,
        JobErrorLimitName_Assets_per_revision
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobErrorLimitName = JobErrorLimitName'
  { fromJobErrorLimitName ::
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

pattern JobErrorLimitName_AWS_Lake_Formation_data_permission_assets_per_revision :: JobErrorLimitName
pattern JobErrorLimitName_AWS_Lake_Formation_data_permission_assets_per_revision = JobErrorLimitName' "AWS Lake Formation data permission assets per revision"

pattern JobErrorLimitName_Amazon_Redshift_datashare_assets_per_revision :: JobErrorLimitName
pattern JobErrorLimitName_Amazon_Redshift_datashare_assets_per_revision = JobErrorLimitName' "Amazon Redshift datashare assets per revision"

pattern JobErrorLimitName_Amazon_S3_data_access_assets_per_revision :: JobErrorLimitName
pattern JobErrorLimitName_Amazon_S3_data_access_assets_per_revision = JobErrorLimitName' "Amazon S3 data access assets per revision"

pattern JobErrorLimitName_Asset_size_in_GB :: JobErrorLimitName
pattern JobErrorLimitName_Asset_size_in_GB = JobErrorLimitName' "Asset size in GB"

pattern JobErrorLimitName_Assets_per_revision :: JobErrorLimitName
pattern JobErrorLimitName_Assets_per_revision = JobErrorLimitName' "Assets per revision"

{-# COMPLETE
  JobErrorLimitName_AWS_Lake_Formation_data_permission_assets_per_revision,
  JobErrorLimitName_Amazon_Redshift_datashare_assets_per_revision,
  JobErrorLimitName_Amazon_S3_data_access_assets_per_revision,
  JobErrorLimitName_Asset_size_in_GB,
  JobErrorLimitName_Assets_per_revision,
  JobErrorLimitName'
  #-}
