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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude

newtype EcrScanFrequency = EcrScanFrequency'
  { fromEcrScanFrequency ::
      Core.Text
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
