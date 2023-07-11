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
-- Module      : Amazonka.QuickSight.Types.BarsArrangement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BarsArrangement
  ( BarsArrangement
      ( ..,
        BarsArrangement_CLUSTERED,
        BarsArrangement_STACKED,
        BarsArrangement_STACKED_PERCENT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BarsArrangement = BarsArrangement'
  { fromBarsArrangement ::
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

pattern BarsArrangement_CLUSTERED :: BarsArrangement
pattern BarsArrangement_CLUSTERED = BarsArrangement' "CLUSTERED"

pattern BarsArrangement_STACKED :: BarsArrangement
pattern BarsArrangement_STACKED = BarsArrangement' "STACKED"

pattern BarsArrangement_STACKED_PERCENT :: BarsArrangement
pattern BarsArrangement_STACKED_PERCENT = BarsArrangement' "STACKED_PERCENT"

{-# COMPLETE
  BarsArrangement_CLUSTERED,
  BarsArrangement_STACKED,
  BarsArrangement_STACKED_PERCENT,
  BarsArrangement'
  #-}
