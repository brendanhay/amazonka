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
-- Module      : Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafInitializationVectorInManifest
  ( CmafInitializationVectorInManifest
      ( ..,
        CmafInitializationVectorInManifest_EXCLUDE,
        CmafInitializationVectorInManifest_INCLUDE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | When you use DRM with CMAF outputs, choose whether the service writes
-- the 128-bit encryption initialization vector in the HLS and DASH
-- manifests.
newtype CmafInitializationVectorInManifest = CmafInitializationVectorInManifest'
  { fromCmafInitializationVectorInManifest ::
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

pattern CmafInitializationVectorInManifest_EXCLUDE :: CmafInitializationVectorInManifest
pattern CmafInitializationVectorInManifest_EXCLUDE = CmafInitializationVectorInManifest' "EXCLUDE"

pattern CmafInitializationVectorInManifest_INCLUDE :: CmafInitializationVectorInManifest
pattern CmafInitializationVectorInManifest_INCLUDE = CmafInitializationVectorInManifest' "INCLUDE"

{-# COMPLETE
  CmafInitializationVectorInManifest_EXCLUDE,
  CmafInitializationVectorInManifest_INCLUDE,
  CmafInitializationVectorInManifest'
  #-}
