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
-- Module      : Amazonka.QuickSight.Types.ArcThicknessOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ArcThicknessOptions
  ( ArcThicknessOptions
      ( ..,
        ArcThicknessOptions_LARGE,
        ArcThicknessOptions_MEDIUM,
        ArcThicknessOptions_SMALL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ArcThicknessOptions = ArcThicknessOptions'
  { fromArcThicknessOptions ::
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

pattern ArcThicknessOptions_LARGE :: ArcThicknessOptions
pattern ArcThicknessOptions_LARGE = ArcThicknessOptions' "LARGE"

pattern ArcThicknessOptions_MEDIUM :: ArcThicknessOptions
pattern ArcThicknessOptions_MEDIUM = ArcThicknessOptions' "MEDIUM"

pattern ArcThicknessOptions_SMALL :: ArcThicknessOptions
pattern ArcThicknessOptions_SMALL = ArcThicknessOptions' "SMALL"

{-# COMPLETE
  ArcThicknessOptions_LARGE,
  ArcThicknessOptions_MEDIUM,
  ArcThicknessOptions_SMALL,
  ArcThicknessOptions'
  #-}
