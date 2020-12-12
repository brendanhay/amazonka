{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
  ( DolbyVisionLevel6Mode
      ( DolbyVisionLevel6Mode',
        Passthrough,
        Recalculate,
        Specify
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision MaxCLL and MaxFALL properies.
newtype DolbyVisionLevel6Mode = DolbyVisionLevel6Mode' Lude.Text
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

pattern Passthrough :: DolbyVisionLevel6Mode
pattern Passthrough = DolbyVisionLevel6Mode' "PASSTHROUGH"

pattern Recalculate :: DolbyVisionLevel6Mode
pattern Recalculate = DolbyVisionLevel6Mode' "RECALCULATE"

pattern Specify :: DolbyVisionLevel6Mode
pattern Specify = DolbyVisionLevel6Mode' "SPECIFY"

{-# COMPLETE
  Passthrough,
  Recalculate,
  Specify,
  DolbyVisionLevel6Mode'
  #-}
