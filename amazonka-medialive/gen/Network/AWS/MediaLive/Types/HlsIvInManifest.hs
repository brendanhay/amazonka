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
-- Module      : Network.AWS.MediaLive.Types.HlsIvInManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsIvInManifest
  ( HlsIvInManifest
      ( ..,
        HlsIvInManifest_EXCLUDE,
        HlsIvInManifest_INCLUDE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Hls Iv In Manifest
newtype HlsIvInManifest = HlsIvInManifest'
  { fromHlsIvInManifest ::
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

pattern HlsIvInManifest_EXCLUDE :: HlsIvInManifest
pattern HlsIvInManifest_EXCLUDE = HlsIvInManifest' "EXCLUDE"

pattern HlsIvInManifest_INCLUDE :: HlsIvInManifest
pattern HlsIvInManifest_INCLUDE = HlsIvInManifest' "INCLUDE"

{-# COMPLETE
  HlsIvInManifest_EXCLUDE,
  HlsIvInManifest_INCLUDE,
  HlsIvInManifest'
  #-}
