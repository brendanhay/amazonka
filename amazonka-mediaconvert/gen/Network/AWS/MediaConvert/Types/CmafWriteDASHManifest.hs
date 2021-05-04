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

import qualified Network.AWS.Prelude as Prelude

-- | When set to ENABLED, a DASH MPD manifest will be generated for this
-- output.
newtype CmafWriteDASHManifest = CmafWriteDASHManifest'
  { fromCmafWriteDASHManifest ::
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

pattern CmafWriteDASHManifest_DISABLED :: CmafWriteDASHManifest
pattern CmafWriteDASHManifest_DISABLED = CmafWriteDASHManifest' "DISABLED"

pattern CmafWriteDASHManifest_ENABLED :: CmafWriteDASHManifest
pattern CmafWriteDASHManifest_ENABLED = CmafWriteDASHManifest' "ENABLED"

{-# COMPLETE
  CmafWriteDASHManifest_DISABLED,
  CmafWriteDASHManifest_ENABLED,
  CmafWriteDASHManifest'
  #-}
