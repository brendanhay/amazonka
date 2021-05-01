{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ConformancePackComplianceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ConformancePackComplianceType
  ( ConformancePackComplianceType
      ( ..,
        ConformancePackComplianceType_COMPLIANT,
        ConformancePackComplianceType_INSUFFICIENT_DATA,
        ConformancePackComplianceType_NON_COMPLIANT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ConformancePackComplianceType = ConformancePackComplianceType'
  { fromConformancePackComplianceType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
