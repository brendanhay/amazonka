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
-- Module      : Network.AWS.MediaLive.Types.HlsRedundantManifest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.HlsRedundantManifest
  ( HlsRedundantManifest
      ( ..,
        HlsRedundantManifest_DISABLED,
        HlsRedundantManifest_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Hls Redundant Manifest
newtype HlsRedundantManifest = HlsRedundantManifest'
  { fromHlsRedundantManifest ::
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

pattern HlsRedundantManifest_DISABLED :: HlsRedundantManifest
pattern HlsRedundantManifest_DISABLED = HlsRedundantManifest' "DISABLED"

pattern HlsRedundantManifest_ENABLED :: HlsRedundantManifest
pattern HlsRedundantManifest_ENABLED = HlsRedundantManifest' "ENABLED"

{-# COMPLETE
  HlsRedundantManifest_DISABLED,
  HlsRedundantManifest_ENABLED,
  HlsRedundantManifest'
  #-}
