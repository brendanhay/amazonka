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
-- Module      : Amazonka.QuickSight.Types.RelativeDateType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RelativeDateType
  ( RelativeDateType
      ( ..,
        RelativeDateType_LAST,
        RelativeDateType_NEXT,
        RelativeDateType_NOW,
        RelativeDateType_PREVIOUS,
        RelativeDateType_THIS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RelativeDateType = RelativeDateType'
  { fromRelativeDateType ::
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

pattern RelativeDateType_LAST :: RelativeDateType
pattern RelativeDateType_LAST = RelativeDateType' "LAST"

pattern RelativeDateType_NEXT :: RelativeDateType
pattern RelativeDateType_NEXT = RelativeDateType' "NEXT"

pattern RelativeDateType_NOW :: RelativeDateType
pattern RelativeDateType_NOW = RelativeDateType' "NOW"

pattern RelativeDateType_PREVIOUS :: RelativeDateType
pattern RelativeDateType_PREVIOUS = RelativeDateType' "PREVIOUS"

pattern RelativeDateType_THIS :: RelativeDateType
pattern RelativeDateType_THIS = RelativeDateType' "THIS"

{-# COMPLETE
  RelativeDateType_LAST,
  RelativeDateType_NEXT,
  RelativeDateType_NOW,
  RelativeDateType_PREVIOUS,
  RelativeDateType_THIS,
  RelativeDateType'
  #-}
