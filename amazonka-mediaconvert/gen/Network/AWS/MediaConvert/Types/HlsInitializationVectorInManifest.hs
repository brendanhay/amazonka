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
-- Module      : Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsInitializationVectorInManifest
  ( HlsInitializationVectorInManifest
      ( ..,
        HlsInitializationVectorInManifest_EXCLUDE,
        HlsInitializationVectorInManifest_INCLUDE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The Initialization Vector is a 128-bit number used in conjunction with
-- the key for encrypting blocks. If set to INCLUDE, Initialization Vector
-- is listed in the manifest. Otherwise Initialization Vector is not in the
-- manifest.
newtype HlsInitializationVectorInManifest = HlsInitializationVectorInManifest'
  { fromHlsInitializationVectorInManifest ::
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

pattern HlsInitializationVectorInManifest_EXCLUDE :: HlsInitializationVectorInManifest
pattern HlsInitializationVectorInManifest_EXCLUDE = HlsInitializationVectorInManifest' "EXCLUDE"

pattern HlsInitializationVectorInManifest_INCLUDE :: HlsInitializationVectorInManifest
pattern HlsInitializationVectorInManifest_INCLUDE = HlsInitializationVectorInManifest' "INCLUDE"

{-# COMPLETE
  HlsInitializationVectorInManifest_EXCLUDE,
  HlsInitializationVectorInManifest_INCLUDE,
  HlsInitializationVectorInManifest'
  #-}
