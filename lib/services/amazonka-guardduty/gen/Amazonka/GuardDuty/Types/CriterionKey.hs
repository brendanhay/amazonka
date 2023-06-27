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
-- Module      : Amazonka.GuardDuty.Types.CriterionKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CriterionKey
  ( CriterionKey
      ( ..,
        CriterionKey_ACCOUNT_ID,
        CriterionKey_EC2_INSTANCE_ARN,
        CriterionKey_GUARDDUTY_FINDING_ID,
        CriterionKey_SCAN_ID,
        CriterionKey_SCAN_START_TIME,
        CriterionKey_SCAN_STATUS,
        CriterionKey_SCAN_TYPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CriterionKey = CriterionKey'
  { fromCriterionKey ::
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

pattern CriterionKey_ACCOUNT_ID :: CriterionKey
pattern CriterionKey_ACCOUNT_ID = CriterionKey' "ACCOUNT_ID"

pattern CriterionKey_EC2_INSTANCE_ARN :: CriterionKey
pattern CriterionKey_EC2_INSTANCE_ARN = CriterionKey' "EC2_INSTANCE_ARN"

pattern CriterionKey_GUARDDUTY_FINDING_ID :: CriterionKey
pattern CriterionKey_GUARDDUTY_FINDING_ID = CriterionKey' "GUARDDUTY_FINDING_ID"

pattern CriterionKey_SCAN_ID :: CriterionKey
pattern CriterionKey_SCAN_ID = CriterionKey' "SCAN_ID"

pattern CriterionKey_SCAN_START_TIME :: CriterionKey
pattern CriterionKey_SCAN_START_TIME = CriterionKey' "SCAN_START_TIME"

pattern CriterionKey_SCAN_STATUS :: CriterionKey
pattern CriterionKey_SCAN_STATUS = CriterionKey' "SCAN_STATUS"

pattern CriterionKey_SCAN_TYPE :: CriterionKey
pattern CriterionKey_SCAN_TYPE = CriterionKey' "SCAN_TYPE"

{-# COMPLETE
  CriterionKey_ACCOUNT_ID,
  CriterionKey_EC2_INSTANCE_ARN,
  CriterionKey_GUARDDUTY_FINDING_ID,
  CriterionKey_SCAN_ID,
  CriterionKey_SCAN_START_TIME,
  CriterionKey_SCAN_STATUS,
  CriterionKey_SCAN_TYPE,
  CriterionKey'
  #-}
