{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.Types.APIName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisVideo.Types.APIName
  ( APIName
      ( ..,
        APIName_GET_CLIP,
        APIName_GET_DASH_STREAMING_SESSION_URL,
        APIName_GET_HLS_STREAMING_SESSION_URL,
        APIName_GET_MEDIA,
        APIName_GET_MEDIA_FOR_FRAGMENT_LIST,
        APIName_LIST_FRAGMENTS,
        APIName_PUT_MEDIA
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype APIName = APIName' {fromAPIName :: Core.Text}
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern APIName_GET_CLIP :: APIName
pattern APIName_GET_CLIP = APIName' "GET_CLIP"

pattern APIName_GET_DASH_STREAMING_SESSION_URL :: APIName
pattern APIName_GET_DASH_STREAMING_SESSION_URL = APIName' "GET_DASH_STREAMING_SESSION_URL"

pattern APIName_GET_HLS_STREAMING_SESSION_URL :: APIName
pattern APIName_GET_HLS_STREAMING_SESSION_URL = APIName' "GET_HLS_STREAMING_SESSION_URL"

pattern APIName_GET_MEDIA :: APIName
pattern APIName_GET_MEDIA = APIName' "GET_MEDIA"

pattern APIName_GET_MEDIA_FOR_FRAGMENT_LIST :: APIName
pattern APIName_GET_MEDIA_FOR_FRAGMENT_LIST = APIName' "GET_MEDIA_FOR_FRAGMENT_LIST"

pattern APIName_LIST_FRAGMENTS :: APIName
pattern APIName_LIST_FRAGMENTS = APIName' "LIST_FRAGMENTS"

pattern APIName_PUT_MEDIA :: APIName
pattern APIName_PUT_MEDIA = APIName' "PUT_MEDIA"

{-# COMPLETE
  APIName_GET_CLIP,
  APIName_GET_DASH_STREAMING_SESSION_URL,
  APIName_GET_HLS_STREAMING_SESSION_URL,
  APIName_GET_MEDIA,
  APIName_GET_MEDIA_FOR_FRAGMENT_LIST,
  APIName_LIST_FRAGMENTS,
  APIName_PUT_MEDIA,
  APIName'
  #-}
