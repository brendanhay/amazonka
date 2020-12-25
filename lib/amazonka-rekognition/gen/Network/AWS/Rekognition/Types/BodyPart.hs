{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.BodyPart
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.BodyPart
  ( BodyPart
      ( BodyPart',
        BodyPartFace,
        BodyPartHead,
        BodyPartLeftHand,
        BodyPartRightHand,
        fromBodyPart
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype BodyPart = BodyPart' {fromBodyPart :: Core.Text}
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

pattern BodyPartFace :: BodyPart
pattern BodyPartFace = BodyPart' "FACE"

pattern BodyPartHead :: BodyPart
pattern BodyPartHead = BodyPart' "HEAD"

pattern BodyPartLeftHand :: BodyPart
pattern BodyPartLeftHand = BodyPart' "LEFT_HAND"

pattern BodyPartRightHand :: BodyPart
pattern BodyPartRightHand = BodyPart' "RIGHT_HAND"

{-# COMPLETE
  BodyPartFace,
  BodyPartHead,
  BodyPartLeftHand,
  BodyPartRightHand,
  BodyPart'
  #-}
