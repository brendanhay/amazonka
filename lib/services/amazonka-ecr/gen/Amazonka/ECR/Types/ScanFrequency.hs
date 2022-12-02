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
-- Module      : Amazonka.ECR.Types.ScanFrequency
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ScanFrequency
  ( ScanFrequency
      ( ..,
        ScanFrequency_CONTINUOUS_SCAN,
        ScanFrequency_MANUAL,
        ScanFrequency_SCAN_ON_PUSH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ScanFrequency = ScanFrequency'
  { fromScanFrequency ::
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

pattern ScanFrequency_CONTINUOUS_SCAN :: ScanFrequency
pattern ScanFrequency_CONTINUOUS_SCAN = ScanFrequency' "CONTINUOUS_SCAN"

pattern ScanFrequency_MANUAL :: ScanFrequency
pattern ScanFrequency_MANUAL = ScanFrequency' "MANUAL"

pattern ScanFrequency_SCAN_ON_PUSH :: ScanFrequency
pattern ScanFrequency_SCAN_ON_PUSH = ScanFrequency' "SCAN_ON_PUSH"

{-# COMPLETE
  ScanFrequency_CONTINUOUS_SCAN,
  ScanFrequency_MANUAL,
  ScanFrequency_SCAN_ON_PUSH,
  ScanFrequency'
  #-}
