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
-- Module      : Amazonka.SSM.Types.ComplianceSeverity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ComplianceSeverity
  ( ComplianceSeverity
      ( ..,
        ComplianceSeverity_CRITICAL,
        ComplianceSeverity_HIGH,
        ComplianceSeverity_INFORMATIONAL,
        ComplianceSeverity_LOW,
        ComplianceSeverity_MEDIUM,
        ComplianceSeverity_UNSPECIFIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComplianceSeverity = ComplianceSeverity'
  { fromComplianceSeverity ::
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

pattern ComplianceSeverity_CRITICAL :: ComplianceSeverity
pattern ComplianceSeverity_CRITICAL = ComplianceSeverity' "CRITICAL"

pattern ComplianceSeverity_HIGH :: ComplianceSeverity
pattern ComplianceSeverity_HIGH = ComplianceSeverity' "HIGH"

pattern ComplianceSeverity_INFORMATIONAL :: ComplianceSeverity
pattern ComplianceSeverity_INFORMATIONAL = ComplianceSeverity' "INFORMATIONAL"

pattern ComplianceSeverity_LOW :: ComplianceSeverity
pattern ComplianceSeverity_LOW = ComplianceSeverity' "LOW"

pattern ComplianceSeverity_MEDIUM :: ComplianceSeverity
pattern ComplianceSeverity_MEDIUM = ComplianceSeverity' "MEDIUM"

pattern ComplianceSeverity_UNSPECIFIED :: ComplianceSeverity
pattern ComplianceSeverity_UNSPECIFIED = ComplianceSeverity' "UNSPECIFIED"

{-# COMPLETE
  ComplianceSeverity_CRITICAL,
  ComplianceSeverity_HIGH,
  ComplianceSeverity_INFORMATIONAL,
  ComplianceSeverity_LOW,
  ComplianceSeverity_MEDIUM,
  ComplianceSeverity_UNSPECIFIED,
  ComplianceSeverity'
  #-}
