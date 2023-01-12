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
-- Module      : Amazonka.CustomerProfiles.Types.SourceConnectorType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.SourceConnectorType
  ( SourceConnectorType
      ( ..,
        SourceConnectorType_Marketo,
        SourceConnectorType_S3,
        SourceConnectorType_Salesforce,
        SourceConnectorType_Servicenow,
        SourceConnectorType_Zendesk
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceConnectorType = SourceConnectorType'
  { fromSourceConnectorType ::
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

pattern SourceConnectorType_Marketo :: SourceConnectorType
pattern SourceConnectorType_Marketo = SourceConnectorType' "Marketo"

pattern SourceConnectorType_S3 :: SourceConnectorType
pattern SourceConnectorType_S3 = SourceConnectorType' "S3"

pattern SourceConnectorType_Salesforce :: SourceConnectorType
pattern SourceConnectorType_Salesforce = SourceConnectorType' "Salesforce"

pattern SourceConnectorType_Servicenow :: SourceConnectorType
pattern SourceConnectorType_Servicenow = SourceConnectorType' "Servicenow"

pattern SourceConnectorType_Zendesk :: SourceConnectorType
pattern SourceConnectorType_Zendesk = SourceConnectorType' "Zendesk"

{-# COMPLETE
  SourceConnectorType_Marketo,
  SourceConnectorType_S3,
  SourceConnectorType_Salesforce,
  SourceConnectorType_Servicenow,
  SourceConnectorType_Zendesk,
  SourceConnectorType'
  #-}
