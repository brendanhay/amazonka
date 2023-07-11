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
-- Module      : Amazonka.IAM.Types.AccessAdvisorUsageGranularityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.AccessAdvisorUsageGranularityType
  ( AccessAdvisorUsageGranularityType
      ( ..,
        AccessAdvisorUsageGranularityType_ACTION_LEVEL,
        AccessAdvisorUsageGranularityType_SERVICE_LEVEL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccessAdvisorUsageGranularityType = AccessAdvisorUsageGranularityType'
  { fromAccessAdvisorUsageGranularityType ::
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

pattern AccessAdvisorUsageGranularityType_ACTION_LEVEL :: AccessAdvisorUsageGranularityType
pattern AccessAdvisorUsageGranularityType_ACTION_LEVEL = AccessAdvisorUsageGranularityType' "ACTION_LEVEL"

pattern AccessAdvisorUsageGranularityType_SERVICE_LEVEL :: AccessAdvisorUsageGranularityType
pattern AccessAdvisorUsageGranularityType_SERVICE_LEVEL = AccessAdvisorUsageGranularityType' "SERVICE_LEVEL"

{-# COMPLETE
  AccessAdvisorUsageGranularityType_ACTION_LEVEL,
  AccessAdvisorUsageGranularityType_SERVICE_LEVEL,
  AccessAdvisorUsageGranularityType'
  #-}
