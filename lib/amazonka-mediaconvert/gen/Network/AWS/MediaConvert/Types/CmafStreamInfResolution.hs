-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafStreamInfResolution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafStreamInfResolution
  ( CmafStreamInfResolution
      ( CmafStreamInfResolution',
        CSIRExclude,
        CSIRInclude
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Include or exclude RESOLUTION attribute for video in EXT-X-STREAM-INF tag of variant manifest.
newtype CmafStreamInfResolution = CmafStreamInfResolution' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern CSIRExclude :: CmafStreamInfResolution
pattern CSIRExclude = CmafStreamInfResolution' "EXCLUDE"

pattern CSIRInclude :: CmafStreamInfResolution
pattern CSIRInclude = CmafStreamInfResolution' "INCLUDE"

{-# COMPLETE
  CSIRExclude,
  CSIRInclude,
  CmafStreamInfResolution'
  #-}
