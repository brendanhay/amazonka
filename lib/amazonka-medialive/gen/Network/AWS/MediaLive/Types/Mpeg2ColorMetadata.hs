{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2ColorMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2ColorMetadata
  ( Mpeg2ColorMetadata
      ( Mpeg2ColorMetadata',
        Mpeg2ColorMetadataIgnore,
        Mpeg2ColorMetadataInsert,
        fromMpeg2ColorMetadata
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Mpeg2 Color Metadata
newtype Mpeg2ColorMetadata = Mpeg2ColorMetadata'
  { fromMpeg2ColorMetadata ::
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

pattern Mpeg2ColorMetadataIgnore :: Mpeg2ColorMetadata
pattern Mpeg2ColorMetadataIgnore = Mpeg2ColorMetadata' "IGNORE"

pattern Mpeg2ColorMetadataInsert :: Mpeg2ColorMetadata
pattern Mpeg2ColorMetadataInsert = Mpeg2ColorMetadata' "INSERT"

{-# COMPLETE
  Mpeg2ColorMetadataIgnore,
  Mpeg2ColorMetadataInsert,
  Mpeg2ColorMetadata'
  #-}
