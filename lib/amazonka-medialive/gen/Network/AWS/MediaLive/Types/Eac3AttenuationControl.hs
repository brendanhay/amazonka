{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3AttenuationControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3AttenuationControl
  ( Eac3AttenuationControl
      ( Eac3AttenuationControl',
        EACAttenuate3DB,
        EACNone
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Eac3 Attenuation Control
newtype Eac3AttenuationControl = Eac3AttenuationControl' Lude.Text
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

pattern EACAttenuate3DB :: Eac3AttenuationControl
pattern EACAttenuate3DB = Eac3AttenuationControl' "ATTENUATE_3_DB"

pattern EACNone :: Eac3AttenuationControl
pattern EACNone = Eac3AttenuationControl' "NONE"

{-# COMPLETE
  EACAttenuate3DB,
  EACNone,
  Eac3AttenuationControl'
  #-}
