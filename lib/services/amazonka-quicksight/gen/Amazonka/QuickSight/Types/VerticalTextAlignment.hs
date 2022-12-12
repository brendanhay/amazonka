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
-- Module      : Amazonka.QuickSight.Types.VerticalTextAlignment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VerticalTextAlignment
  ( VerticalTextAlignment
      ( ..,
        VerticalTextAlignment_BOTTOM,
        VerticalTextAlignment_MIDDLE,
        VerticalTextAlignment_TOP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype VerticalTextAlignment = VerticalTextAlignment'
  { fromVerticalTextAlignment ::
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

pattern VerticalTextAlignment_BOTTOM :: VerticalTextAlignment
pattern VerticalTextAlignment_BOTTOM = VerticalTextAlignment' "BOTTOM"

pattern VerticalTextAlignment_MIDDLE :: VerticalTextAlignment
pattern VerticalTextAlignment_MIDDLE = VerticalTextAlignment' "MIDDLE"

pattern VerticalTextAlignment_TOP :: VerticalTextAlignment
pattern VerticalTextAlignment_TOP = VerticalTextAlignment' "TOP"

{-# COMPLETE
  VerticalTextAlignment_BOTTOM,
  VerticalTextAlignment_MIDDLE,
  VerticalTextAlignment_TOP,
  VerticalTextAlignment'
  #-}
