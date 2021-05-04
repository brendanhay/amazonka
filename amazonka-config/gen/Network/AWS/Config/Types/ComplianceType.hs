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
-- Module      : Network.AWS.Config.Types.ComplianceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceType
  ( ComplianceType
      ( ..,
        ComplianceType_COMPLIANT,
        ComplianceType_INSUFFICIENT_DATA,
        ComplianceType_NON_COMPLIANT,
        ComplianceType_NOT_APPLICABLE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ComplianceType = ComplianceType'
  { fromComplianceType ::
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

pattern ComplianceType_COMPLIANT :: ComplianceType
pattern ComplianceType_COMPLIANT = ComplianceType' "COMPLIANT"

pattern ComplianceType_INSUFFICIENT_DATA :: ComplianceType
pattern ComplianceType_INSUFFICIENT_DATA = ComplianceType' "INSUFFICIENT_DATA"

pattern ComplianceType_NON_COMPLIANT :: ComplianceType
pattern ComplianceType_NON_COMPLIANT = ComplianceType' "NON_COMPLIANT"

pattern ComplianceType_NOT_APPLICABLE :: ComplianceType
pattern ComplianceType_NOT_APPLICABLE = ComplianceType' "NOT_APPLICABLE"

{-# COMPLETE
  ComplianceType_COMPLIANT,
  ComplianceType_INSUFFICIENT_DATA,
  ComplianceType_NON_COMPLIANT,
  ComplianceType_NOT_APPLICABLE,
  ComplianceType'
  #-}
