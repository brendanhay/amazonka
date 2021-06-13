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
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteHLSManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafWriteHLSManifest
  ( CmafWriteHLSManifest
      ( ..,
        CmafWriteHLSManifest_DISABLED,
        CmafWriteHLSManifest_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | When set to ENABLED, an Apple HLS manifest will be generated for this
-- output.
newtype CmafWriteHLSManifest = CmafWriteHLSManifest'
  { fromCmafWriteHLSManifest ::
      Core.Text
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

pattern CmafWriteHLSManifest_DISABLED :: CmafWriteHLSManifest
pattern CmafWriteHLSManifest_DISABLED = CmafWriteHLSManifest' "DISABLED"

pattern CmafWriteHLSManifest_ENABLED :: CmafWriteHLSManifest
pattern CmafWriteHLSManifest_ENABLED = CmafWriteHLSManifest' "ENABLED"

{-# COMPLETE
  CmafWriteHLSManifest_DISABLED,
  CmafWriteHLSManifest_ENABLED,
  CmafWriteHLSManifest'
  #-}
