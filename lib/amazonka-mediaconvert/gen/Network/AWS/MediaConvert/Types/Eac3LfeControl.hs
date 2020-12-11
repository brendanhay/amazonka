-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3LfeControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3LfeControl
  ( Eac3LfeControl
      ( Eac3LfeControl',
        ELCLfe,
        ELCNoLfe
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When encoding 3/2 audio, controls whether the LFE channel is enabled
newtype Eac3LfeControl = Eac3LfeControl' Lude.Text
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

pattern ELCLfe :: Eac3LfeControl
pattern ELCLfe = Eac3LfeControl' "LFE"

pattern ELCNoLfe :: Eac3LfeControl
pattern ELCNoLfe = Eac3LfeControl' "NO_LFE"

{-# COMPLETE
  ELCLfe,
  ELCNoLfe,
  Eac3LfeControl'
  #-}
