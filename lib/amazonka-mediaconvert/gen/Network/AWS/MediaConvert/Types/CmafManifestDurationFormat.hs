{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafManifestDurationFormat
  ( CmafManifestDurationFormat
    ( CmafManifestDurationFormat'
    , CmafManifestDurationFormatFloatingPoint
    , CmafManifestDurationFormatInteger
    , fromCmafManifestDurationFormat
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Indicates whether the output manifest should use floating point values for segment duration.
newtype CmafManifestDurationFormat = CmafManifestDurationFormat'{fromCmafManifestDurationFormat
                                                                 :: Core.Text}
                                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                       Core.Generic)
                                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                         Core.ToJSONKey, Core.FromJSONKey,
                                                         Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                         Core.FromXML, Core.ToText, Core.FromText,
                                                         Core.ToByteString, Core.ToQuery,
                                                         Core.ToHeader)

pattern CmafManifestDurationFormatFloatingPoint :: CmafManifestDurationFormat
pattern CmafManifestDurationFormatFloatingPoint = CmafManifestDurationFormat' "FLOATING_POINT"

pattern CmafManifestDurationFormatInteger :: CmafManifestDurationFormat
pattern CmafManifestDurationFormatInteger = CmafManifestDurationFormat' "INTEGER"

{-# COMPLETE 
  CmafManifestDurationFormatFloatingPoint,

  CmafManifestDurationFormatInteger,
  CmafManifestDurationFormat'
  #-}
