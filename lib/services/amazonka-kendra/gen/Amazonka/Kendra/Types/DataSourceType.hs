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
-- Module      : Amazonka.Kendra.Types.DataSourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.DataSourceType
  ( DataSourceType
      ( ..,
        DataSourceType_ALFRESCO,
        DataSourceType_BOX,
        DataSourceType_CONFLUENCE,
        DataSourceType_CUSTOM,
        DataSourceType_DATABASE,
        DataSourceType_FSX,
        DataSourceType_GITHUB,
        DataSourceType_GOOGLEDRIVE,
        DataSourceType_JIRA,
        DataSourceType_ONEDRIVE,
        DataSourceType_QUIP,
        DataSourceType_S3,
        DataSourceType_SALESFORCE,
        DataSourceType_SERVICENOW,
        DataSourceType_SHAREPOINT,
        DataSourceType_SLACK,
        DataSourceType_TEMPLATE,
        DataSourceType_WEBCRAWLER,
        DataSourceType_WORKDOCS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataSourceType = DataSourceType'
  { fromDataSourceType ::
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

pattern DataSourceType_ALFRESCO :: DataSourceType
pattern DataSourceType_ALFRESCO = DataSourceType' "ALFRESCO"

pattern DataSourceType_BOX :: DataSourceType
pattern DataSourceType_BOX = DataSourceType' "BOX"

pattern DataSourceType_CONFLUENCE :: DataSourceType
pattern DataSourceType_CONFLUENCE = DataSourceType' "CONFLUENCE"

pattern DataSourceType_CUSTOM :: DataSourceType
pattern DataSourceType_CUSTOM = DataSourceType' "CUSTOM"

pattern DataSourceType_DATABASE :: DataSourceType
pattern DataSourceType_DATABASE = DataSourceType' "DATABASE"

pattern DataSourceType_FSX :: DataSourceType
pattern DataSourceType_FSX = DataSourceType' "FSX"

pattern DataSourceType_GITHUB :: DataSourceType
pattern DataSourceType_GITHUB = DataSourceType' "GITHUB"

pattern DataSourceType_GOOGLEDRIVE :: DataSourceType
pattern DataSourceType_GOOGLEDRIVE = DataSourceType' "GOOGLEDRIVE"

pattern DataSourceType_JIRA :: DataSourceType
pattern DataSourceType_JIRA = DataSourceType' "JIRA"

pattern DataSourceType_ONEDRIVE :: DataSourceType
pattern DataSourceType_ONEDRIVE = DataSourceType' "ONEDRIVE"

pattern DataSourceType_QUIP :: DataSourceType
pattern DataSourceType_QUIP = DataSourceType' "QUIP"

pattern DataSourceType_S3 :: DataSourceType
pattern DataSourceType_S3 = DataSourceType' "S3"

pattern DataSourceType_SALESFORCE :: DataSourceType
pattern DataSourceType_SALESFORCE = DataSourceType' "SALESFORCE"

pattern DataSourceType_SERVICENOW :: DataSourceType
pattern DataSourceType_SERVICENOW = DataSourceType' "SERVICENOW"

pattern DataSourceType_SHAREPOINT :: DataSourceType
pattern DataSourceType_SHAREPOINT = DataSourceType' "SHAREPOINT"

pattern DataSourceType_SLACK :: DataSourceType
pattern DataSourceType_SLACK = DataSourceType' "SLACK"

pattern DataSourceType_TEMPLATE :: DataSourceType
pattern DataSourceType_TEMPLATE = DataSourceType' "TEMPLATE"

pattern DataSourceType_WEBCRAWLER :: DataSourceType
pattern DataSourceType_WEBCRAWLER = DataSourceType' "WEBCRAWLER"

pattern DataSourceType_WORKDOCS :: DataSourceType
pattern DataSourceType_WORKDOCS = DataSourceType' "WORKDOCS"

{-# COMPLETE
  DataSourceType_ALFRESCO,
  DataSourceType_BOX,
  DataSourceType_CONFLUENCE,
  DataSourceType_CUSTOM,
  DataSourceType_DATABASE,
  DataSourceType_FSX,
  DataSourceType_GITHUB,
  DataSourceType_GOOGLEDRIVE,
  DataSourceType_JIRA,
  DataSourceType_ONEDRIVE,
  DataSourceType_QUIP,
  DataSourceType_S3,
  DataSourceType_SALESFORCE,
  DataSourceType_SERVICENOW,
  DataSourceType_SHAREPOINT,
  DataSourceType_SLACK,
  DataSourceType_TEMPLATE,
  DataSourceType_WEBCRAWLER,
  DataSourceType_WORKDOCS,
  DataSourceType'
  #-}
