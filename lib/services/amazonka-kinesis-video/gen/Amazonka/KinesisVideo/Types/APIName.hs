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
-- Module      : Amazonka.KinesisVideo.Types.APIName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisVideo.Types.APIName
  ( APIName
      ( ..,
        APIName_GET_CLIP,
        APIName_GET_DASH_STREAMING_SESSION_URL,
        APIName_GET_HLS_STREAMING_SESSION_URL,
        APIName_GET_IMAGES,
        APIName_GET_MEDIA,
        APIName_GET_MEDIA_FOR_FRAGMENT_LIST,
        APIName_LIST_FRAGMENTS,
        APIName_PUT_MEDIA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype APIName = APIName' {fromAPIName :: Data.Text}
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern APIName_GET_CLIP :: APIName
pattern APIName_GET_CLIP = APIName' "GET_CLIP"

pattern APIName_GET_DASH_STREAMING_SESSION_URL :: APIName
pattern APIName_GET_DASH_STREAMING_SESSION_URL = APIName' "GET_DASH_STREAMING_SESSION_URL"

pattern APIName_GET_HLS_STREAMING_SESSION_URL :: APIName
pattern APIName_GET_HLS_STREAMING_SESSION_URL = APIName' "GET_HLS_STREAMING_SESSION_URL"

pattern APIName_GET_IMAGES :: APIName
pattern APIName_GET_IMAGES = APIName' "GET_IMAGES"

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
  APIName_GET_IMAGES,
  APIName_GET_MEDIA,
  APIName_GET_MEDIA_FOR_FRAGMENT_LIST,
  APIName_LIST_FRAGMENTS,
  APIName_PUT_MEDIA,
  APIName'
  #-}
