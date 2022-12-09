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
-- Module      : Amazonka.QuickSight.Types.LayoutElementType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LayoutElementType
  ( LayoutElementType
      ( ..,
        LayoutElementType_FILTER_CONTROL,
        LayoutElementType_PARAMETER_CONTROL,
        LayoutElementType_TEXT_BOX,
        LayoutElementType_VISUAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LayoutElementType = LayoutElementType'
  { fromLayoutElementType ::
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

pattern LayoutElementType_FILTER_CONTROL :: LayoutElementType
pattern LayoutElementType_FILTER_CONTROL = LayoutElementType' "FILTER_CONTROL"

pattern LayoutElementType_PARAMETER_CONTROL :: LayoutElementType
pattern LayoutElementType_PARAMETER_CONTROL = LayoutElementType' "PARAMETER_CONTROL"

pattern LayoutElementType_TEXT_BOX :: LayoutElementType
pattern LayoutElementType_TEXT_BOX = LayoutElementType' "TEXT_BOX"

pattern LayoutElementType_VISUAL :: LayoutElementType
pattern LayoutElementType_VISUAL = LayoutElementType' "VISUAL"

{-# COMPLETE
  LayoutElementType_FILTER_CONTROL,
  LayoutElementType_PARAMETER_CONTROL,
  LayoutElementType_TEXT_BOX,
  LayoutElementType_VISUAL,
  LayoutElementType'
  #-}
