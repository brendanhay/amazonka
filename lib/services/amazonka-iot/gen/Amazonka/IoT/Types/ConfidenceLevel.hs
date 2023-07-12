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
-- Module      : Amazonka.IoT.Types.ConfidenceLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ConfidenceLevel
  ( ConfidenceLevel
      ( ..,
        ConfidenceLevel_HIGH,
        ConfidenceLevel_LOW,
        ConfidenceLevel_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfidenceLevel = ConfidenceLevel'
  { fromConfidenceLevel ::
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

pattern ConfidenceLevel_HIGH :: ConfidenceLevel
pattern ConfidenceLevel_HIGH = ConfidenceLevel' "HIGH"

pattern ConfidenceLevel_LOW :: ConfidenceLevel
pattern ConfidenceLevel_LOW = ConfidenceLevel' "LOW"

pattern ConfidenceLevel_MEDIUM :: ConfidenceLevel
pattern ConfidenceLevel_MEDIUM = ConfidenceLevel' "MEDIUM"

{-# COMPLETE
  ConfidenceLevel_HIGH,
  ConfidenceLevel_LOW,
  ConfidenceLevel_MEDIUM,
  ConfidenceLevel'
  #-}
