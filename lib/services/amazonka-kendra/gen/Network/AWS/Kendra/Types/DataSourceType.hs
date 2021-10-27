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
-- Module      : Network.AWS.Kendra.Types.DataSourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.DataSourceType
  ( DataSourceType
      ( ..,
        DataSourceType_CONFLUENCE,
        DataSourceType_CUSTOM,
        DataSourceType_DATABASE,
        DataSourceType_GOOGLEDRIVE,
        DataSourceType_ONEDRIVE,
        DataSourceType_S3,
        DataSourceType_SALESFORCE,
        DataSourceType_SERVICENOW,
        DataSourceType_SHAREPOINT,
        DataSourceType_WEBCRAWLER,
        DataSourceType_WORKDOCS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype DataSourceType = DataSourceType'
  { fromDataSourceType ::
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

pattern DataSourceType_CONFLUENCE :: DataSourceType
pattern DataSourceType_CONFLUENCE = DataSourceType' "CONFLUENCE"

pattern DataSourceType_CUSTOM :: DataSourceType
pattern DataSourceType_CUSTOM = DataSourceType' "CUSTOM"

pattern DataSourceType_DATABASE :: DataSourceType
pattern DataSourceType_DATABASE = DataSourceType' "DATABASE"

pattern DataSourceType_GOOGLEDRIVE :: DataSourceType
pattern DataSourceType_GOOGLEDRIVE = DataSourceType' "GOOGLEDRIVE"

pattern DataSourceType_ONEDRIVE :: DataSourceType
pattern DataSourceType_ONEDRIVE = DataSourceType' "ONEDRIVE"

pattern DataSourceType_S3 :: DataSourceType
pattern DataSourceType_S3 = DataSourceType' "S3"

pattern DataSourceType_SALESFORCE :: DataSourceType
pattern DataSourceType_SALESFORCE = DataSourceType' "SALESFORCE"

pattern DataSourceType_SERVICENOW :: DataSourceType
pattern DataSourceType_SERVICENOW = DataSourceType' "SERVICENOW"

pattern DataSourceType_SHAREPOINT :: DataSourceType
pattern DataSourceType_SHAREPOINT = DataSourceType' "SHAREPOINT"

pattern DataSourceType_WEBCRAWLER :: DataSourceType
pattern DataSourceType_WEBCRAWLER = DataSourceType' "WEBCRAWLER"

pattern DataSourceType_WORKDOCS :: DataSourceType
pattern DataSourceType_WORKDOCS = DataSourceType' "WORKDOCS"

{-# COMPLETE
  DataSourceType_CONFLUENCE,
  DataSourceType_CUSTOM,
  DataSourceType_DATABASE,
  DataSourceType_GOOGLEDRIVE,
  DataSourceType_ONEDRIVE,
  DataSourceType_S3,
  DataSourceType_SALESFORCE,
  DataSourceType_SERVICENOW,
  DataSourceType_SHAREPOINT,
  DataSourceType_WEBCRAWLER,
  DataSourceType_WORKDOCS,
  DataSourceType'
  #-}
