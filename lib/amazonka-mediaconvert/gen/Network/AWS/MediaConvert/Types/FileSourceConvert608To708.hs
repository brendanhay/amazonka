{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.FileSourceConvert608To708
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.FileSourceConvert608To708
  ( FileSourceConvert608To708
      ( FileSourceConvert608To708',
        FileSourceConvert608To708Upconvert,
        FileSourceConvert608To708Disabled,
        fromFileSourceConvert608To708
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify whether this set of input captions appears in your outputs in both 608 and 708 format. If you choose Upconvert (UPCONVERT), MediaConvert includes the captions data in two ways: it passes the 608 data through using the 608 compatibility bytes fields of the 708 wrapper, and it also translates the 608 data into 708.
newtype FileSourceConvert608To708 = FileSourceConvert608To708'
  { fromFileSourceConvert608To708 ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern FileSourceConvert608To708Upconvert :: FileSourceConvert608To708
pattern FileSourceConvert608To708Upconvert = FileSourceConvert608To708' "UPCONVERT"

pattern FileSourceConvert608To708Disabled :: FileSourceConvert608To708
pattern FileSourceConvert608To708Disabled = FileSourceConvert608To708' "DISABLED"

{-# COMPLETE
  FileSourceConvert608To708Upconvert,
  FileSourceConvert608To708Disabled,
  FileSourceConvert608To708'
  #-}
