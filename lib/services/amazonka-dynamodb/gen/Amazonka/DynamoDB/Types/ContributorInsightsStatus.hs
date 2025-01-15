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
-- Module      : Amazonka.DynamoDB.Types.ContributorInsightsStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ContributorInsightsStatus
  ( ContributorInsightsStatus
      ( ..,
        ContributorInsightsStatus_DISABLED,
        ContributorInsightsStatus_DISABLING,
        ContributorInsightsStatus_ENABLED,
        ContributorInsightsStatus_ENABLING,
        ContributorInsightsStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.TransactWriteItem
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

newtype ContributorInsightsStatus = ContributorInsightsStatus'
  { fromContributorInsightsStatus ::
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

pattern ContributorInsightsStatus_DISABLED :: ContributorInsightsStatus
pattern ContributorInsightsStatus_DISABLED = ContributorInsightsStatus' "DISABLED"

pattern ContributorInsightsStatus_DISABLING :: ContributorInsightsStatus
pattern ContributorInsightsStatus_DISABLING = ContributorInsightsStatus' "DISABLING"

pattern ContributorInsightsStatus_ENABLED :: ContributorInsightsStatus
pattern ContributorInsightsStatus_ENABLED = ContributorInsightsStatus' "ENABLED"

pattern ContributorInsightsStatus_ENABLING :: ContributorInsightsStatus
pattern ContributorInsightsStatus_ENABLING = ContributorInsightsStatus' "ENABLING"

pattern ContributorInsightsStatus_FAILED :: ContributorInsightsStatus
pattern ContributorInsightsStatus_FAILED = ContributorInsightsStatus' "FAILED"

{-# COMPLETE
  ContributorInsightsStatus_DISABLED,
  ContributorInsightsStatus_DISABLING,
  ContributorInsightsStatus_ENABLED,
  ContributorInsightsStatus_ENABLING,
  ContributorInsightsStatus_FAILED,
  ContributorInsightsStatus'
  #-}
