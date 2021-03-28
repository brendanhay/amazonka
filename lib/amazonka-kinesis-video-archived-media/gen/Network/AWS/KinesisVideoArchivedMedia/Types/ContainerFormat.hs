{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.ContainerFormat
  ( ContainerFormat
    ( ContainerFormat'
    , ContainerFormatFragmentedMP4
    , ContainerFormatMpegTs
    , fromContainerFormat
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ContainerFormat = ContainerFormat'{fromContainerFormat ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern ContainerFormatFragmentedMP4 :: ContainerFormat
pattern ContainerFormatFragmentedMP4 = ContainerFormat' "FRAGMENTED_MP4"

pattern ContainerFormatMpegTs :: ContainerFormat
pattern ContainerFormatMpegTs = ContainerFormat' "MPEG_TS"

{-# COMPLETE 
  ContainerFormatFragmentedMP4,

  ContainerFormatMpegTs,
  ContainerFormat'
  #-}
