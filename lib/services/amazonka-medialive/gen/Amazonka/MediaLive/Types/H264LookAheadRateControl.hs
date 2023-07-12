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
-- Module      : Amazonka.MediaLive.Types.H264LookAheadRateControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.H264LookAheadRateControl
  ( H264LookAheadRateControl
      ( ..,
        H264LookAheadRateControl_HIGH,
        H264LookAheadRateControl_LOW,
        H264LookAheadRateControl_MEDIUM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | H264 Look Ahead Rate Control
newtype H264LookAheadRateControl = H264LookAheadRateControl'
  { fromH264LookAheadRateControl ::
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

pattern H264LookAheadRateControl_HIGH :: H264LookAheadRateControl
pattern H264LookAheadRateControl_HIGH = H264LookAheadRateControl' "HIGH"

pattern H264LookAheadRateControl_LOW :: H264LookAheadRateControl
pattern H264LookAheadRateControl_LOW = H264LookAheadRateControl' "LOW"

pattern H264LookAheadRateControl_MEDIUM :: H264LookAheadRateControl
pattern H264LookAheadRateControl_MEDIUM = H264LookAheadRateControl' "MEDIUM"

{-# COMPLETE
  H264LookAheadRateControl_HIGH,
  H264LookAheadRateControl_LOW,
  H264LookAheadRateControl_MEDIUM,
  H264LookAheadRateControl'
  #-}
