{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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
        BurninSubtitleAlignment_CENTERED,
        BurninSubtitleAlignment_LEFT
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | If no explicit x_position or y_position is provided, setting alignment
-- to centered will place the captions at the bottom center of the output.
-- Similarly, setting a left alignment will align captions to the bottom
-- left of the output. If x and y positions are given in conjunction with
-- the alignment parameter, the font will be justified (either left or
-- centered) relative to those coordinates. This option is not valid for
-- source captions that are STL, 608\/embedded or teletext. These source
-- settings are already pre-defined by the caption stream. All burn-in and
-- DVB-Sub font settings must match.
newtype BurninSubtitleAlignment = BurninSubtitleAlignment'
  { fromBurninSubtitleAlignment ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern BurninSubtitleAlignment_CENTERED :: BurninSubtitleAlignment
pattern BurninSubtitleAlignment_CENTERED = BurninSubtitleAlignment' "CENTERED"

pattern BurninSubtitleAlignment_LEFT :: BurninSubtitleAlignment
pattern BurninSubtitleAlignment_LEFT = BurninSubtitleAlignment' "LEFT"

{-# COMPLETE
  BurninSubtitleAlignment_CENTERED,
  BurninSubtitleAlignment_LEFT,
  BurninSubtitleAlignment'
  #-}
