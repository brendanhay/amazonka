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
-- Module      : Amazonka.MediaConvert.Types.CmafInitializationVectorInManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmafInitializationVectorInManifest
  ( CmafInitializationVectorInManifest
      ( ..,
        CmafInitializationVectorInManifest_EXCLUDE,
        CmafInitializationVectorInManifest_INCLUDE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When you use DRM with CMAF outputs, choose whether the service writes
-- the 128-bit encryption initialization vector in the HLS and DASH
-- manifests.
newtype CmafInitializationVectorInManifest = CmafInitializationVectorInManifest'
  { fromCmafInitializationVectorInManifest ::
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

pattern CmafInitializationVectorInManifest_EXCLUDE :: CmafInitializationVectorInManifest
pattern CmafInitializationVectorInManifest_EXCLUDE = CmafInitializationVectorInManifest' "EXCLUDE"

pattern CmafInitializationVectorInManifest_INCLUDE :: CmafInitializationVectorInManifest
pattern CmafInitializationVectorInManifest_INCLUDE = CmafInitializationVectorInManifest' "INCLUDE"

{-# COMPLETE
  CmafInitializationVectorInManifest_EXCLUDE,
  CmafInitializationVectorInManifest_INCLUDE,
  CmafInitializationVectorInManifest'
  #-}
