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
-- Module      : Amazonka.QuickSight.Types.HorizontalTextAlignment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HorizontalTextAlignment
  ( HorizontalTextAlignment
      ( ..,
        HorizontalTextAlignment_AUTO,
        HorizontalTextAlignment_CENTER,
        HorizontalTextAlignment_LEFT,
        HorizontalTextAlignment_RIGHT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HorizontalTextAlignment = HorizontalTextAlignment'
  { fromHorizontalTextAlignment ::
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

pattern HorizontalTextAlignment_AUTO :: HorizontalTextAlignment
pattern HorizontalTextAlignment_AUTO = HorizontalTextAlignment' "AUTO"

pattern HorizontalTextAlignment_CENTER :: HorizontalTextAlignment
pattern HorizontalTextAlignment_CENTER = HorizontalTextAlignment' "CENTER"

pattern HorizontalTextAlignment_LEFT :: HorizontalTextAlignment
pattern HorizontalTextAlignment_LEFT = HorizontalTextAlignment' "LEFT"

pattern HorizontalTextAlignment_RIGHT :: HorizontalTextAlignment
pattern HorizontalTextAlignment_RIGHT = HorizontalTextAlignment' "RIGHT"

{-# COMPLETE
  HorizontalTextAlignment_AUTO,
  HorizontalTextAlignment_CENTER,
  HorizontalTextAlignment_LEFT,
  HorizontalTextAlignment_RIGHT,
  HorizontalTextAlignment'
  #-}
