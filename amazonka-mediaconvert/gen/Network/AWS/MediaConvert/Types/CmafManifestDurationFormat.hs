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
-- Module      : Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
  ( CmafManifestDurationFormat
      ( ..,
        CmafManifestDurationFormat_FLOATING_POINT,
        CmafManifestDurationFormat_INTEGER
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether the output manifest should use floating point values
-- for segment duration.
newtype CmafManifestDurationFormat = CmafManifestDurationFormat'
  { fromCmafManifestDurationFormat ::
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

pattern CmafManifestDurationFormat_FLOATING_POINT :: CmafManifestDurationFormat
pattern CmafManifestDurationFormat_FLOATING_POINT = CmafManifestDurationFormat' "FLOATING_POINT"

pattern CmafManifestDurationFormat_INTEGER :: CmafManifestDurationFormat
pattern CmafManifestDurationFormat_INTEGER = CmafManifestDurationFormat' "INTEGER"

{-# COMPLETE
  CmafManifestDurationFormat_FLOATING_POINT,
  CmafManifestDurationFormat_INTEGER,
  CmafManifestDurationFormat'
  #-}
