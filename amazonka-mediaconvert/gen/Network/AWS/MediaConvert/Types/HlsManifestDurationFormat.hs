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
-- Module      : Network.AWS.MediaConvert.Types.HlsManifestDurationFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HlsManifestDurationFormat
  ( HlsManifestDurationFormat
      ( ..,
        HlsManifestDurationFormat_FLOATING_POINT,
        HlsManifestDurationFormat_INTEGER
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Indicates whether the output manifest should use floating point values
-- for segment duration.
newtype HlsManifestDurationFormat = HlsManifestDurationFormat'
  { fromHlsManifestDurationFormat ::
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

pattern HlsManifestDurationFormat_FLOATING_POINT :: HlsManifestDurationFormat
pattern HlsManifestDurationFormat_FLOATING_POINT = HlsManifestDurationFormat' "FLOATING_POINT"

pattern HlsManifestDurationFormat_INTEGER :: HlsManifestDurationFormat
pattern HlsManifestDurationFormat_INTEGER = HlsManifestDurationFormat' "INTEGER"

{-# COMPLETE
  HlsManifestDurationFormat_FLOATING_POINT,
  HlsManifestDurationFormat_INTEGER,
  HlsManifestDurationFormat'
  #-}
