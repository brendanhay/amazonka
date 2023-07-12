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
-- Module      : Amazonka.MediaConvert.Types.CmafWriteHLSManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmafWriteHLSManifest
  ( CmafWriteHLSManifest
      ( ..,
        CmafWriteHLSManifest_DISABLED,
        CmafWriteHLSManifest_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When set to ENABLED, an Apple HLS manifest will be generated for this
-- output.
newtype CmafWriteHLSManifest = CmafWriteHLSManifest'
  { fromCmafWriteHLSManifest ::
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

pattern CmafWriteHLSManifest_DISABLED :: CmafWriteHLSManifest
pattern CmafWriteHLSManifest_DISABLED = CmafWriteHLSManifest' "DISABLED"

pattern CmafWriteHLSManifest_ENABLED :: CmafWriteHLSManifest
pattern CmafWriteHLSManifest_ENABLED = CmafWriteHLSManifest' "ENABLED"

{-# COMPLETE
  CmafWriteHLSManifest_DISABLED,
  CmafWriteHLSManifest_ENABLED,
  CmafWriteHLSManifest'
  #-}
