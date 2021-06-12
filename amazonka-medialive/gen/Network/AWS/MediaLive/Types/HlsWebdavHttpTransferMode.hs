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
-- Module      : Network.AWS.MediaLive.Types.HlsWebdavHttpTransferMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsWebdavHttpTransferMode
  ( HlsWebdavHttpTransferMode
      ( ..,
        HlsWebdavHttpTransferMode_CHUNKED,
        HlsWebdavHttpTransferMode_NON_CHUNKED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Hls Webdav Http Transfer Mode
newtype HlsWebdavHttpTransferMode = HlsWebdavHttpTransferMode'
  { fromHlsWebdavHttpTransferMode ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern HlsWebdavHttpTransferMode_CHUNKED :: HlsWebdavHttpTransferMode
pattern HlsWebdavHttpTransferMode_CHUNKED = HlsWebdavHttpTransferMode' "CHUNKED"

pattern HlsWebdavHttpTransferMode_NON_CHUNKED :: HlsWebdavHttpTransferMode
pattern HlsWebdavHttpTransferMode_NON_CHUNKED = HlsWebdavHttpTransferMode' "NON_CHUNKED"

{-# COMPLETE
  HlsWebdavHttpTransferMode_CHUNKED,
  HlsWebdavHttpTransferMode_NON_CHUNKED,
  HlsWebdavHttpTransferMode'
  #-}
