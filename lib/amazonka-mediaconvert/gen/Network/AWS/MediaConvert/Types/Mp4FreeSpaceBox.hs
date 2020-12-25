{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mp4FreeSpaceBox
  ( Mp4FreeSpaceBox
      ( Mp4FreeSpaceBox',
        Mp4FreeSpaceBoxInclude,
        Mp4FreeSpaceBoxExclude,
        fromMp4FreeSpaceBox
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Inserts a free-space box immediately after the moov box.
newtype Mp4FreeSpaceBox = Mp4FreeSpaceBox'
  { fromMp4FreeSpaceBox ::
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

pattern Mp4FreeSpaceBoxInclude :: Mp4FreeSpaceBox
pattern Mp4FreeSpaceBoxInclude = Mp4FreeSpaceBox' "INCLUDE"

pattern Mp4FreeSpaceBoxExclude :: Mp4FreeSpaceBox
pattern Mp4FreeSpaceBoxExclude = Mp4FreeSpaceBox' "EXCLUDE"

{-# COMPLETE
  Mp4FreeSpaceBoxInclude,
  Mp4FreeSpaceBoxExclude,
  Mp4FreeSpaceBox'
  #-}
