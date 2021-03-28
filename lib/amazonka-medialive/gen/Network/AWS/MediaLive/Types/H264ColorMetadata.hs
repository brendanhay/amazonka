{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264ColorMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.H264ColorMetadata
  ( H264ColorMetadata
    ( H264ColorMetadata'
    , H264ColorMetadataIgnore
    , H264ColorMetadataInsert
    , fromH264ColorMetadata
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | H264 Color Metadata
newtype H264ColorMetadata = H264ColorMetadata'{fromH264ColorMetadata
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern H264ColorMetadataIgnore :: H264ColorMetadata
pattern H264ColorMetadataIgnore = H264ColorMetadata' "IGNORE"

pattern H264ColorMetadataInsert :: H264ColorMetadata
pattern H264ColorMetadataInsert = H264ColorMetadata' "INSERT"

{-# COMPLETE 
  H264ColorMetadataIgnore,

  H264ColorMetadataInsert,
  H264ColorMetadata'
  #-}
