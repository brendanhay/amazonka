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
-- Module      : Network.AWS.SSM.Types.ComplianceQueryOperatorType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ComplianceQueryOperatorType
  ( ComplianceQueryOperatorType
      ( ..,
        ComplianceQueryOperatorType_BEGIN_WITH,
        ComplianceQueryOperatorType_EQUAL,
        ComplianceQueryOperatorType_GREATER_THAN,
        ComplianceQueryOperatorType_LESS_THAN,
        ComplianceQueryOperatorType_NOT_EQUAL
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ComplianceQueryOperatorType = ComplianceQueryOperatorType'
  { fromComplianceQueryOperatorType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ComplianceQueryOperatorType_BEGIN_WITH :: ComplianceQueryOperatorType
pattern ComplianceQueryOperatorType_BEGIN_WITH = ComplianceQueryOperatorType' "BEGIN_WITH"

pattern ComplianceQueryOperatorType_EQUAL :: ComplianceQueryOperatorType
pattern ComplianceQueryOperatorType_EQUAL = ComplianceQueryOperatorType' "EQUAL"

pattern ComplianceQueryOperatorType_GREATER_THAN :: ComplianceQueryOperatorType
pattern ComplianceQueryOperatorType_GREATER_THAN = ComplianceQueryOperatorType' "GREATER_THAN"

pattern ComplianceQueryOperatorType_LESS_THAN :: ComplianceQueryOperatorType
pattern ComplianceQueryOperatorType_LESS_THAN = ComplianceQueryOperatorType' "LESS_THAN"

pattern ComplianceQueryOperatorType_NOT_EQUAL :: ComplianceQueryOperatorType
pattern ComplianceQueryOperatorType_NOT_EQUAL = ComplianceQueryOperatorType' "NOT_EQUAL"

{-# COMPLETE
  ComplianceQueryOperatorType_BEGIN_WITH,
  ComplianceQueryOperatorType_EQUAL,
  ComplianceQueryOperatorType_GREATER_THAN,
  ComplianceQueryOperatorType_LESS_THAN,
  ComplianceQueryOperatorType_NOT_EQUAL,
  ComplianceQueryOperatorType'
  #-}
