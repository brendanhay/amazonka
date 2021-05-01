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

import qualified Network.AWS.Prelude as Prelude

-- | The Initialization Vector is a 128-bit number used in conjunction with
-- the key for encrypting blocks. If set to INCLUDE, Initialization Vector
-- is listed in the manifest. Otherwise Initialization Vector is not in the
-- manifest.
newtype HlsInitializationVectorInManifest = HlsInitializationVectorInManifest'
  { fromHlsInitializationVectorInManifest ::
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

pattern HlsInitializationVectorInManifest_EXCLUDE :: HlsInitializationVectorInManifest
pattern HlsInitializationVectorInManifest_EXCLUDE = HlsInitializationVectorInManifest' "EXCLUDE"

pattern HlsInitializationVectorInManifest_INCLUDE :: HlsInitializationVectorInManifest
pattern HlsInitializationVectorInManifest_INCLUDE = HlsInitializationVectorInManifest' "INCLUDE"

{-# COMPLETE
  HlsInitializationVectorInManifest_EXCLUDE,
  HlsInitializationVectorInManifest_INCLUDE,
  HlsInitializationVectorInManifest'
  #-}
