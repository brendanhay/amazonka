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
-- Module      : Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DvbSubtitleBackgroundColor
  ( DvbSubtitleBackgroundColor
      ( ..,
        DvbSubtitleBackgroundColor_BLACK,
        DvbSubtitleBackgroundColor_NONE,
        DvbSubtitleBackgroundColor_WHITE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specifies the color of the rectangle behind the captions. All burn-in
-- and DVB-Sub font settings must match.
newtype DvbSubtitleBackgroundColor = DvbSubtitleBackgroundColor'
  { fromDvbSubtitleBackgroundColor ::
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

pattern DvbSubtitleBackgroundColor_BLACK :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_BLACK = DvbSubtitleBackgroundColor' "BLACK"

pattern DvbSubtitleBackgroundColor_NONE :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_NONE = DvbSubtitleBackgroundColor' "NONE"

pattern DvbSubtitleBackgroundColor_WHITE :: DvbSubtitleBackgroundColor
pattern DvbSubtitleBackgroundColor_WHITE = DvbSubtitleBackgroundColor' "WHITE"

{-# COMPLETE
  DvbSubtitleBackgroundColor_BLACK,
  DvbSubtitleBackgroundColor_NONE,
  DvbSubtitleBackgroundColor_WHITE,
  DvbSubtitleBackgroundColor'
  #-}
