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
-- Module      : Amazonka.Inspector2.Types.EcrScanFrequency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.EcrScanFrequency
  ( EcrScanFrequency
      ( ..,
        EcrScanFrequency_CONTINUOUS_SCAN,
        EcrScanFrequency_MANUAL,
        EcrScanFrequency_SCAN_ON_PUSH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EcrScanFrequency = EcrScanFrequency'
  { fromEcrScanFrequency ::
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

pattern EcrScanFrequency_CONTINUOUS_SCAN :: EcrScanFrequency
pattern EcrScanFrequency_CONTINUOUS_SCAN = EcrScanFrequency' "CONTINUOUS_SCAN"

pattern EcrScanFrequency_MANUAL :: EcrScanFrequency
pattern EcrScanFrequency_MANUAL = EcrScanFrequency' "MANUAL"

pattern EcrScanFrequency_SCAN_ON_PUSH :: EcrScanFrequency
pattern EcrScanFrequency_SCAN_ON_PUSH = EcrScanFrequency' "SCAN_ON_PUSH"

{-# COMPLETE
  EcrScanFrequency_CONTINUOUS_SCAN,
  EcrScanFrequency_MANUAL,
  EcrScanFrequency_SCAN_ON_PUSH,
  EcrScanFrequency'
  #-}
