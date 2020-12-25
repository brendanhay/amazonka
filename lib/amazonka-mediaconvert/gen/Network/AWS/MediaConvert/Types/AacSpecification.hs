{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AacSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AacSpecification
  ( AacSpecification
      ( AacSpecification',
        AacSpecificationMPEG2,
        AacSpecificationMPEG4,
        fromAacSpecification
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Use MPEG-2 AAC instead of MPEG-4 AAC audio for raw or MPEG-2 Transport Stream containers.
newtype AacSpecification = AacSpecification'
  { fromAacSpecification ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern AacSpecificationMPEG2 :: AacSpecification
pattern AacSpecificationMPEG2 = AacSpecification' "MPEG2"

pattern AacSpecificationMPEG4 :: AacSpecification
pattern AacSpecificationMPEG4 = AacSpecification' "MPEG4"

{-# COMPLETE
  AacSpecificationMPEG2,
  AacSpecificationMPEG4,
  AacSpecification'
  #-}
