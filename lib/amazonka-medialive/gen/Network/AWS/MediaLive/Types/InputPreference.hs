{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputPreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.InputPreference
  ( InputPreference
    ( InputPreference'
    , InputPreferenceEqualInputPreference
    , InputPreferencePrimaryInputPreferred
    , fromInputPreference
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Input preference when deciding which input to make active when a previously failed input has recovered.
--
-- If \"EQUAL_INPUT_PREFERENCE\", then the active input will stay active as long as it is healthy.
-- If \"PRIMARY_INPUT_PREFERRED\", then always switch back to the primary input when it is healthy.
newtype InputPreference = InputPreference'{fromInputPreference ::
                                           Core.Text}
                            deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                            Core.Generic)
                            deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                              Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                              Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                              Core.FromText, Core.ToByteString, Core.ToQuery,
                                              Core.ToHeader)

pattern InputPreferenceEqualInputPreference :: InputPreference
pattern InputPreferenceEqualInputPreference = InputPreference' "EQUAL_INPUT_PREFERENCE"

pattern InputPreferencePrimaryInputPreferred :: InputPreference
pattern InputPreferencePrimaryInputPreferred = InputPreference' "PRIMARY_INPUT_PREFERRED"

{-# COMPLETE 
  InputPreferenceEqualInputPreference,

  InputPreferencePrimaryInputPreferred,
  InputPreference'
  #-}
