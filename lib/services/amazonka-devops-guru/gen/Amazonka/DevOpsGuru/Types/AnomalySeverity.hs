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
-- Module      : Amazonka.DevOpsGuru.Types.AnomalySeverity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AnomalySeverity
  ( AnomalySeverity
      ( ..,
        AnomalySeverity_HIGH,
        AnomalySeverity_LOW,
        AnomalySeverity_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AnomalySeverity = AnomalySeverity'
  { fromAnomalySeverity ::
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

pattern AnomalySeverity_HIGH :: AnomalySeverity
pattern AnomalySeverity_HIGH = AnomalySeverity' "HIGH"

pattern AnomalySeverity_LOW :: AnomalySeverity
pattern AnomalySeverity_LOW = AnomalySeverity' "LOW"

pattern AnomalySeverity_MEDIUM :: AnomalySeverity
pattern AnomalySeverity_MEDIUM = AnomalySeverity' "MEDIUM"

{-# COMPLETE
  AnomalySeverity_HIGH,
  AnomalySeverity_LOW,
  AnomalySeverity_MEDIUM,
  AnomalySeverity'
  #-}
