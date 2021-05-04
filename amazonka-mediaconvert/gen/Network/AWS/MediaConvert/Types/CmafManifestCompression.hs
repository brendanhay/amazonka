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

import qualified Network.AWS.Prelude as Prelude

-- | When set to GZIP, compresses HLS playlist.
newtype CmafManifestCompression = CmafManifestCompression'
  { fromCmafManifestCompression ::
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

pattern CmafManifestCompression_GZIP :: CmafManifestCompression
pattern CmafManifestCompression_GZIP = CmafManifestCompression' "GZIP"

pattern CmafManifestCompression_NONE :: CmafManifestCompression
pattern CmafManifestCompression_NONE = CmafManifestCompression' "NONE"

{-# COMPLETE
  CmafManifestCompression_GZIP,
  CmafManifestCompression_NONE,
  CmafManifestCompression'
  #-}
