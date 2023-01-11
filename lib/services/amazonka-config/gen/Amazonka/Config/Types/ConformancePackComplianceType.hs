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
-- Module      : Amazonka.Config.Types.ConformancePackComplianceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConformancePackComplianceType
  ( ConformancePackComplianceType
      ( ..,
        ConformancePackComplianceType_COMPLIANT,
        ConformancePackComplianceType_INSUFFICIENT_DATA,
        ConformancePackComplianceType_NON_COMPLIANT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConformancePackComplianceType = ConformancePackComplianceType'
  { fromConformancePackComplianceType ::
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

pattern ConformancePackComplianceType_COMPLIANT :: ConformancePackComplianceType
pattern ConformancePackComplianceType_COMPLIANT = ConformancePackComplianceType' "COMPLIANT"

pattern ConformancePackComplianceType_INSUFFICIENT_DATA :: ConformancePackComplianceType
pattern ConformancePackComplianceType_INSUFFICIENT_DATA = ConformancePackComplianceType' "INSUFFICIENT_DATA"

pattern ConformancePackComplianceType_NON_COMPLIANT :: ConformancePackComplianceType
pattern ConformancePackComplianceType_NON_COMPLIANT = ConformancePackComplianceType' "NON_COMPLIANT"

{-# COMPLETE
  ConformancePackComplianceType_COMPLIANT,
  ConformancePackComplianceType_INSUFFICIENT_DATA,
  ConformancePackComplianceType_NON_COMPLIANT,
  ConformancePackComplianceType'
  #-}
