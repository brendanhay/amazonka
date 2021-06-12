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
-- Module      : Network.AWS.MediaConvert.Types.CmafManifestCompression
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafManifestCompression
  ( CmafManifestCompression
      ( ..,
        CmafManifestCompression_GZIP,
        CmafManifestCompression_NONE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | When set to GZIP, compresses HLS playlist.
newtype CmafManifestCompression = CmafManifestCompression'
  { fromCmafManifestCompression ::
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

pattern CmafManifestCompression_GZIP :: CmafManifestCompression
pattern CmafManifestCompression_GZIP = CmafManifestCompression' "GZIP"

pattern CmafManifestCompression_NONE :: CmafManifestCompression
pattern CmafManifestCompression_NONE = CmafManifestCompression' "NONE"

{-# COMPLETE
  CmafManifestCompression_GZIP,
  CmafManifestCompression_NONE,
  CmafManifestCompression'
  #-}
