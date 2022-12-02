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
-- Module      : Amazonka.SSM.Types.ComplianceQueryOperatorType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ComplianceQueryOperatorType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComplianceQueryOperatorType = ComplianceQueryOperatorType'
  { fromComplianceQueryOperatorType ::
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
