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
-- Module      : Amazonka.Glue.Types.StartingPosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.StartingPosition
  ( StartingPosition
      ( ..,
        StartingPosition_Earliest,
        StartingPosition_Latest,
        StartingPosition_Timestamp,
        StartingPosition_Trim_horizon
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StartingPosition = StartingPosition'
  { fromStartingPosition ::
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

pattern StartingPosition_Earliest :: StartingPosition
pattern StartingPosition_Earliest = StartingPosition' "earliest"

pattern StartingPosition_Latest :: StartingPosition
pattern StartingPosition_Latest = StartingPosition' "latest"

pattern StartingPosition_Timestamp :: StartingPosition
pattern StartingPosition_Timestamp = StartingPosition' "timestamp"

pattern StartingPosition_Trim_horizon :: StartingPosition
pattern StartingPosition_Trim_horizon = StartingPosition' "trim_horizon"

{-# COMPLETE
  StartingPosition_Earliest,
  StartingPosition_Latest,
  StartingPosition_Timestamp,
  StartingPosition_Trim_horizon,
  StartingPosition'
  #-}
