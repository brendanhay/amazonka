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
-- Module      : Network.AWS.DataExchange.Types.JobErrorLimitName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataExchange.Types.JobErrorLimitName
  ( JobErrorLimitName
      ( ..,
        JobErrorLimitName_Amazon_Redshift_datashare_assets_per_revision,
        JobErrorLimitName_Asset_size_in_GB,
        JobErrorLimitName_Assets_per_revision
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The name of the limit that was reached.
newtype JobErrorLimitName = JobErrorLimitName'
  { fromJobErrorLimitName ::
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

pattern JobErrorLimitName_Amazon_Redshift_datashare_assets_per_revision :: JobErrorLimitName
pattern JobErrorLimitName_Amazon_Redshift_datashare_assets_per_revision = JobErrorLimitName' "Amazon Redshift datashare assets per revision"

pattern JobErrorLimitName_Asset_size_in_GB :: JobErrorLimitName
pattern JobErrorLimitName_Asset_size_in_GB = JobErrorLimitName' "Asset size in GB"

pattern JobErrorLimitName_Assets_per_revision :: JobErrorLimitName
pattern JobErrorLimitName_Assets_per_revision = JobErrorLimitName' "Assets per revision"

{-# COMPLETE
  JobErrorLimitName_Amazon_Redshift_datashare_assets_per_revision,
  JobErrorLimitName_Asset_size_in_GB,
  JobErrorLimitName_Assets_per_revision,
  JobErrorLimitName'
  #-}
