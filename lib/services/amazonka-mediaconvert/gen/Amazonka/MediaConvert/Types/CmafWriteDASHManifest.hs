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
-- Module      : Amazonka.MediaConvert.Types.CmafWriteDASHManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmafWriteDASHManifest
  ( CmafWriteDASHManifest
      ( ..,
        CmafWriteDASHManifest_DISABLED,
        CmafWriteDASHManifest_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | When set to ENABLED, a DASH MPD manifest will be generated for this
-- output.
newtype CmafWriteDASHManifest = CmafWriteDASHManifest'
  { fromCmafWriteDASHManifest ::
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

pattern CmafWriteDASHManifest_DISABLED :: CmafWriteDASHManifest
pattern CmafWriteDASHManifest_DISABLED = CmafWriteDASHManifest' "DISABLED"

pattern CmafWriteDASHManifest_ENABLED :: CmafWriteDASHManifest
pattern CmafWriteDASHManifest_ENABLED = CmafWriteDASHManifest' "ENABLED"

{-# COMPLETE
  CmafWriteDASHManifest_DISABLED,
  CmafWriteDASHManifest_ENABLED,
  CmafWriteDASHManifest'
  #-}
