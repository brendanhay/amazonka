{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.APIName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideo.Types.APIName
  ( APIName
    ( APIName'
    , APINamePutMedia
    , APINameGetMedia
    , APINameListFragments
    , APINameGetMediaForFragmentList
    , APINameGetHlsStreamingSessionUrl
    , APINameGetDashStreamingSessionUrl
    , APINameGetClip
    , fromAPIName
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype APIName = APIName'{fromAPIName :: Core.Text}
                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                    Core.Generic)
                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON, Core.FromJSON,
                                      Core.ToXML, Core.FromXML, Core.ToText, Core.FromText,
                                      Core.ToByteString, Core.ToQuery, Core.ToHeader)

pattern APINamePutMedia :: APIName
pattern APINamePutMedia = APIName' "PUT_MEDIA"

pattern APINameGetMedia :: APIName
pattern APINameGetMedia = APIName' "GET_MEDIA"

pattern APINameListFragments :: APIName
pattern APINameListFragments = APIName' "LIST_FRAGMENTS"

pattern APINameGetMediaForFragmentList :: APIName
pattern APINameGetMediaForFragmentList = APIName' "GET_MEDIA_FOR_FRAGMENT_LIST"

pattern APINameGetHlsStreamingSessionUrl :: APIName
pattern APINameGetHlsStreamingSessionUrl = APIName' "GET_HLS_STREAMING_SESSION_URL"

pattern APINameGetDashStreamingSessionUrl :: APIName
pattern APINameGetDashStreamingSessionUrl = APIName' "GET_DASH_STREAMING_SESSION_URL"

pattern APINameGetClip :: APIName
pattern APINameGetClip = APIName' "GET_CLIP"

{-# COMPLETE 
  APINamePutMedia,

  APINameGetMedia,

  APINameListFragments,

  APINameGetMediaForFragmentList,

  APINameGetHlsStreamingSessionUrl,

  APINameGetDashStreamingSessionUrl,

  APINameGetClip,
  APIName'
  #-}
