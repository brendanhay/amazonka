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
-- Module      : Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafWriteDASHManifest
  ( CmafWriteDASHManifest
      ( ..,
        CmafWriteDASHManifest_DISABLED,
        CmafWriteDASHManifest_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | When set to ENABLED, a DASH MPD manifest will be generated for this
-- output.
newtype CmafWriteDASHManifest = CmafWriteDASHManifest'
  { fromCmafWriteDASHManifest ::
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

pattern CmafWriteDASHManifest_DISABLED :: CmafWriteDASHManifest
pattern CmafWriteDASHManifest_DISABLED = CmafWriteDASHManifest' "DISABLED"

pattern CmafWriteDASHManifest_ENABLED :: CmafWriteDASHManifest
pattern CmafWriteDASHManifest_ENABLED = CmafWriteDASHManifest' "ENABLED"

{-# COMPLETE
  CmafWriteDASHManifest_DISABLED,
  CmafWriteDASHManifest_ENABLED,
  CmafWriteDASHManifest'
  #-}
