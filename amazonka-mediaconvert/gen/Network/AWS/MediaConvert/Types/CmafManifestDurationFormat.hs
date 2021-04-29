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

import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether the output manifest should use floating point values
-- for segment duration.
newtype CmafManifestDurationFormat = CmafManifestDurationFormat'
  { fromCmafManifestDurationFormat ::
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

pattern CmafManifestDurationFormat_FLOATING_POINT :: CmafManifestDurationFormat
pattern CmafManifestDurationFormat_FLOATING_POINT = CmafManifestDurationFormat' "FLOATING_POINT"

pattern CmafManifestDurationFormat_INTEGER :: CmafManifestDurationFormat
pattern CmafManifestDurationFormat_INTEGER = CmafManifestDurationFormat' "INTEGER"

{-# COMPLETE
  CmafManifestDurationFormat_FLOATING_POINT,
  CmafManifestDurationFormat_INTEGER,
  CmafManifestDurationFormat'
  #-}
