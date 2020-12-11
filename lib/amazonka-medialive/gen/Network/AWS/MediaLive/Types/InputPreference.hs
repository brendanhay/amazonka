-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputPreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputPreference
  ( InputPreference
      ( InputPreference',
        EqualInputPreference,
        PrimaryInputPreferred
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Input preference when deciding which input to make active when a previously failed input has recovered.
--
-- If \"EQUAL_INPUT_PREFERENCE\", then the active input will stay active as long as it is healthy.
-- If \"PRIMARY_INPUT_PREFERRED\", then always switch back to the primary input when it is healthy.
newtype InputPreference = InputPreference' Lude.Text
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

pattern EqualInputPreference :: InputPreference
pattern EqualInputPreference = InputPreference' "EQUAL_INPUT_PREFERENCE"

pattern PrimaryInputPreferred :: InputPreference
pattern PrimaryInputPreferred = InputPreference' "PRIMARY_INPUT_PREFERRED"

{-# COMPLETE
  EqualInputPreference,
  PrimaryInputPreferred,
  InputPreference'
  #-}
