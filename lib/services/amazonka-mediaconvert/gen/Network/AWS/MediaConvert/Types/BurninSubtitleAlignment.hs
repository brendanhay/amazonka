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
-- Module      : Network.AWS.MediaConvert.Types.BurninSubtitleAlignment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.BurninSubtitleAlignment
  ( BurninSubtitleAlignment
      ( ..,
        BurninSubtitleAlignment_AUTO,
        BurninSubtitleAlignment_CENTERED,
        BurninSubtitleAlignment_LEFT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify the alignment of your captions. If no explicit x_position is
-- provided, setting alignment to centered will placethe captions at the
-- bottom center of the output. Similarly, setting a left alignment
-- willalign captions to the bottom left of the output. If x and y
-- positions are given in conjunction with the alignment parameter, the
-- font will be justified (either left or centered) relative to those
-- coordinates.
newtype BurninSubtitleAlignment = BurninSubtitleAlignment'
  { fromBurninSubtitleAlignment ::
      Core.Text
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
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern BurninSubtitleAlignment_AUTO :: BurninSubtitleAlignment
pattern BurninSubtitleAlignment_AUTO = BurninSubtitleAlignment' "AUTO"

pattern BurninSubtitleAlignment_CENTERED :: BurninSubtitleAlignment
pattern BurninSubtitleAlignment_CENTERED = BurninSubtitleAlignment' "CENTERED"

pattern BurninSubtitleAlignment_LEFT :: BurninSubtitleAlignment
pattern BurninSubtitleAlignment_LEFT = BurninSubtitleAlignment' "LEFT"

{-# COMPLETE
  BurninSubtitleAlignment_AUTO,
  BurninSubtitleAlignment_CENTERED,
  BurninSubtitleAlignment_LEFT,
  BurninSubtitleAlignment'
  #-}
