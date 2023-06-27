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
-- Module      : Amazonka.MediaConvert.Types.CaptionSourceConvertPaintOnToPopOn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CaptionSourceConvertPaintOnToPopOn
  ( CaptionSourceConvertPaintOnToPopOn
      ( ..,
        CaptionSourceConvertPaintOnToPopOn_DISABLED,
        CaptionSourceConvertPaintOnToPopOn_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose the presentation style of your input SCC captions. To use the
-- same presentation style as your input: Keep the default value, Disabled.
-- To convert paint-on captions to pop-on: Choose Enabled. We also
-- recommend that you choose Enabled if you notice additional repeated
-- lines in your output captions.
newtype CaptionSourceConvertPaintOnToPopOn = CaptionSourceConvertPaintOnToPopOn'
  { fromCaptionSourceConvertPaintOnToPopOn ::
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

pattern CaptionSourceConvertPaintOnToPopOn_DISABLED :: CaptionSourceConvertPaintOnToPopOn
pattern CaptionSourceConvertPaintOnToPopOn_DISABLED = CaptionSourceConvertPaintOnToPopOn' "DISABLED"

pattern CaptionSourceConvertPaintOnToPopOn_ENABLED :: CaptionSourceConvertPaintOnToPopOn
pattern CaptionSourceConvertPaintOnToPopOn_ENABLED = CaptionSourceConvertPaintOnToPopOn' "ENABLED"

{-# COMPLETE
  CaptionSourceConvertPaintOnToPopOn_DISABLED,
  CaptionSourceConvertPaintOnToPopOn_ENABLED,
  CaptionSourceConvertPaintOnToPopOn'
  #-}
