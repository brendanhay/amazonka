{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.APIName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.APIName
  ( APIName
      ( APIName',
        PutMedia,
        GetMedia,
        ListFragments,
        GetMediaForFragmentList,
        GetHlsStreamingSessionURL,
        GetDashStreamingSessionURL,
        GetClip
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype APIName = APIName' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern PutMedia :: APIName
pattern PutMedia = APIName' "PUT_MEDIA"

pattern GetMedia :: APIName
pattern GetMedia = APIName' "GET_MEDIA"

pattern ListFragments :: APIName
pattern ListFragments = APIName' "LIST_FRAGMENTS"

pattern GetMediaForFragmentList :: APIName
pattern GetMediaForFragmentList = APIName' "GET_MEDIA_FOR_FRAGMENT_LIST"

pattern GetHlsStreamingSessionURL :: APIName
pattern GetHlsStreamingSessionURL = APIName' "GET_HLS_STREAMING_SESSION_URL"

pattern GetDashStreamingSessionURL :: APIName
pattern GetDashStreamingSessionURL = APIName' "GET_DASH_STREAMING_SESSION_URL"

pattern GetClip :: APIName
pattern GetClip = APIName' "GET_CLIP"

{-# COMPLETE
  PutMedia,
  GetMedia,
  ListFragments,
  GetMediaForFragmentList,
  GetHlsStreamingSessionURL,
  GetDashStreamingSessionURL,
  GetClip,
  APIName'
  #-}
