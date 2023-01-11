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
-- Module      : Amazonka.Kendra.Types.SalesforceStandardObjectName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.SalesforceStandardObjectName
  ( SalesforceStandardObjectName
      ( ..,
        SalesforceStandardObjectName_ACCOUNT,
        SalesforceStandardObjectName_CAMPAIGN,
        SalesforceStandardObjectName_CASE,
        SalesforceStandardObjectName_CONTACT,
        SalesforceStandardObjectName_CONTRACT,
        SalesforceStandardObjectName_DOCUMENT,
        SalesforceStandardObjectName_GROUP,
        SalesforceStandardObjectName_IDEA,
        SalesforceStandardObjectName_LEAD,
        SalesforceStandardObjectName_OPPORTUNITY,
        SalesforceStandardObjectName_PARTNER,
        SalesforceStandardObjectName_PRICEBOOK,
        SalesforceStandardObjectName_PRODUCT,
        SalesforceStandardObjectName_PROFILE,
        SalesforceStandardObjectName_SOLUTION,
        SalesforceStandardObjectName_TASK,
        SalesforceStandardObjectName_USER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SalesforceStandardObjectName = SalesforceStandardObjectName'
  { fromSalesforceStandardObjectName ::
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

pattern SalesforceStandardObjectName_ACCOUNT :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_ACCOUNT = SalesforceStandardObjectName' "ACCOUNT"

pattern SalesforceStandardObjectName_CAMPAIGN :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_CAMPAIGN = SalesforceStandardObjectName' "CAMPAIGN"

pattern SalesforceStandardObjectName_CASE :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_CASE = SalesforceStandardObjectName' "CASE"

pattern SalesforceStandardObjectName_CONTACT :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_CONTACT = SalesforceStandardObjectName' "CONTACT"

pattern SalesforceStandardObjectName_CONTRACT :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_CONTRACT = SalesforceStandardObjectName' "CONTRACT"

pattern SalesforceStandardObjectName_DOCUMENT :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_DOCUMENT = SalesforceStandardObjectName' "DOCUMENT"

pattern SalesforceStandardObjectName_GROUP :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_GROUP = SalesforceStandardObjectName' "GROUP"

pattern SalesforceStandardObjectName_IDEA :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_IDEA = SalesforceStandardObjectName' "IDEA"

pattern SalesforceStandardObjectName_LEAD :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_LEAD = SalesforceStandardObjectName' "LEAD"

pattern SalesforceStandardObjectName_OPPORTUNITY :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_OPPORTUNITY = SalesforceStandardObjectName' "OPPORTUNITY"

pattern SalesforceStandardObjectName_PARTNER :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_PARTNER = SalesforceStandardObjectName' "PARTNER"

pattern SalesforceStandardObjectName_PRICEBOOK :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_PRICEBOOK = SalesforceStandardObjectName' "PRICEBOOK"

pattern SalesforceStandardObjectName_PRODUCT :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_PRODUCT = SalesforceStandardObjectName' "PRODUCT"

pattern SalesforceStandardObjectName_PROFILE :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_PROFILE = SalesforceStandardObjectName' "PROFILE"

pattern SalesforceStandardObjectName_SOLUTION :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_SOLUTION = SalesforceStandardObjectName' "SOLUTION"

pattern SalesforceStandardObjectName_TASK :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_TASK = SalesforceStandardObjectName' "TASK"

pattern SalesforceStandardObjectName_USER :: SalesforceStandardObjectName
pattern SalesforceStandardObjectName_USER = SalesforceStandardObjectName' "USER"

{-# COMPLETE
  SalesforceStandardObjectName_ACCOUNT,
  SalesforceStandardObjectName_CAMPAIGN,
  SalesforceStandardObjectName_CASE,
  SalesforceStandardObjectName_CONTACT,
  SalesforceStandardObjectName_CONTRACT,
  SalesforceStandardObjectName_DOCUMENT,
  SalesforceStandardObjectName_GROUP,
  SalesforceStandardObjectName_IDEA,
  SalesforceStandardObjectName_LEAD,
  SalesforceStandardObjectName_OPPORTUNITY,
  SalesforceStandardObjectName_PARTNER,
  SalesforceStandardObjectName_PRICEBOOK,
  SalesforceStandardObjectName_PRODUCT,
  SalesforceStandardObjectName_PROFILE,
  SalesforceStandardObjectName_SOLUTION,
  SalesforceStandardObjectName_TASK,
  SalesforceStandardObjectName_USER,
  SalesforceStandardObjectName'
  #-}
