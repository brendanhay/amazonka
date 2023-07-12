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
-- Module      : Amazonka.QuickSight.Types.LineInterpolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineInterpolation
  ( LineInterpolation
      ( ..,
        LineInterpolation_LINEAR,
        LineInterpolation_SMOOTH,
        LineInterpolation_STEPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LineInterpolation = LineInterpolation'
  { fromLineInterpolation ::
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

pattern LineInterpolation_LINEAR :: LineInterpolation
pattern LineInterpolation_LINEAR = LineInterpolation' "LINEAR"

pattern LineInterpolation_SMOOTH :: LineInterpolation
pattern LineInterpolation_SMOOTH = LineInterpolation' "SMOOTH"

pattern LineInterpolation_STEPPED :: LineInterpolation
pattern LineInterpolation_STEPPED = LineInterpolation' "STEPPED"

{-# COMPLETE
  LineInterpolation_LINEAR,
  LineInterpolation_SMOOTH,
  LineInterpolation_STEPPED,
  LineInterpolation'
  #-}
