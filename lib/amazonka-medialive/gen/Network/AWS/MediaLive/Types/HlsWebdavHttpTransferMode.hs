{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsWebdavHttpTransferMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.HlsWebdavHttpTransferMode
  ( HlsWebdavHttpTransferMode
    ( HlsWebdavHttpTransferMode'
    , HlsWebdavHttpTransferModeChunked
    , HlsWebdavHttpTransferModeNonChunked
    , fromHlsWebdavHttpTransferMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Hls Webdav Http Transfer Mode
newtype HlsWebdavHttpTransferMode = HlsWebdavHttpTransferMode'{fromHlsWebdavHttpTransferMode
                                                               :: Core.Text}
                                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                      Core.Generic)
                                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                        Core.ToJSONKey, Core.FromJSONKey,
                                                        Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                        Core.FromXML, Core.ToText, Core.FromText,
                                                        Core.ToByteString, Core.ToQuery,
                                                        Core.ToHeader)

pattern HlsWebdavHttpTransferModeChunked :: HlsWebdavHttpTransferMode
pattern HlsWebdavHttpTransferModeChunked = HlsWebdavHttpTransferMode' "CHUNKED"

pattern HlsWebdavHttpTransferModeNonChunked :: HlsWebdavHttpTransferMode
pattern HlsWebdavHttpTransferModeNonChunked = HlsWebdavHttpTransferMode' "NON_CHUNKED"

{-# COMPLETE 
  HlsWebdavHttpTransferModeChunked,

  HlsWebdavHttpTransferModeNonChunked,
  HlsWebdavHttpTransferMode'
  #-}
