{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.HlsAkamaiHttpTransferMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsAkamaiHttpTransferMode
  ( HlsAkamaiHttpTransferMode
      ( ..,
        HlsAkamaiHttpTransferMode_CHUNKED,
        HlsAkamaiHttpTransferMode_NON_CHUNKED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Hls Akamai Http Transfer Mode
newtype HlsAkamaiHttpTransferMode = HlsAkamaiHttpTransferMode'
  { fromHlsAkamaiHttpTransferMode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern HlsAkamaiHttpTransferMode_CHUNKED :: HlsAkamaiHttpTransferMode
pattern HlsAkamaiHttpTransferMode_CHUNKED = HlsAkamaiHttpTransferMode' "CHUNKED"

pattern HlsAkamaiHttpTransferMode_NON_CHUNKED :: HlsAkamaiHttpTransferMode
pattern HlsAkamaiHttpTransferMode_NON_CHUNKED = HlsAkamaiHttpTransferMode' "NON_CHUNKED"

{-# COMPLETE
  HlsAkamaiHttpTransferMode_CHUNKED,
  HlsAkamaiHttpTransferMode_NON_CHUNKED,
  HlsAkamaiHttpTransferMode'
  #-}
