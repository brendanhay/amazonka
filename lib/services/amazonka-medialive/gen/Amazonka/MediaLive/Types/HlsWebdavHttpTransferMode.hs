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
-- Module      : Amazonka.MediaLive.Types.HlsWebdavHttpTransferMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsWebdavHttpTransferMode
  ( HlsWebdavHttpTransferMode
      ( ..,
        HlsWebdavHttpTransferMode_CHUNKED,
        HlsWebdavHttpTransferMode_NON_CHUNKED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Hls Webdav Http Transfer Mode
newtype HlsWebdavHttpTransferMode = HlsWebdavHttpTransferMode'
  { fromHlsWebdavHttpTransferMode ::
      Data.Text
  }
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

pattern HlsWebdavHttpTransferMode_CHUNKED :: HlsWebdavHttpTransferMode
pattern HlsWebdavHttpTransferMode_CHUNKED = HlsWebdavHttpTransferMode' "CHUNKED"

pattern HlsWebdavHttpTransferMode_NON_CHUNKED :: HlsWebdavHttpTransferMode
pattern HlsWebdavHttpTransferMode_NON_CHUNKED = HlsWebdavHttpTransferMode' "NON_CHUNKED"

{-# COMPLETE
  HlsWebdavHttpTransferMode_CHUNKED,
  HlsWebdavHttpTransferMode_NON_CHUNKED,
  HlsWebdavHttpTransferMode'
  #-}
