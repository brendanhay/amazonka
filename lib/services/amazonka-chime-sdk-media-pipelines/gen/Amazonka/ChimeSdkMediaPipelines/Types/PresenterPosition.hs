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
-- Module      : Amazonka.ChimeSdkMediaPipelines.Types.PresenterPosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSdkMediaPipelines.Types.PresenterPosition
  ( PresenterPosition
      ( ..,
        PresenterPosition_BottomLeft,
        PresenterPosition_BottomRight,
        PresenterPosition_TopLeft,
        PresenterPosition_TopRight
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PresenterPosition = PresenterPosition'
  { fromPresenterPosition ::
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

pattern PresenterPosition_BottomLeft :: PresenterPosition
pattern PresenterPosition_BottomLeft = PresenterPosition' "BottomLeft"

pattern PresenterPosition_BottomRight :: PresenterPosition
pattern PresenterPosition_BottomRight = PresenterPosition' "BottomRight"

pattern PresenterPosition_TopLeft :: PresenterPosition
pattern PresenterPosition_TopLeft = PresenterPosition' "TopLeft"

pattern PresenterPosition_TopRight :: PresenterPosition
pattern PresenterPosition_TopRight = PresenterPosition' "TopRight"

{-# COMPLETE
  PresenterPosition_BottomLeft,
  PresenterPosition_BottomRight,
  PresenterPosition_TopLeft,
  PresenterPosition_TopRight,
  PresenterPosition'
  #-}
