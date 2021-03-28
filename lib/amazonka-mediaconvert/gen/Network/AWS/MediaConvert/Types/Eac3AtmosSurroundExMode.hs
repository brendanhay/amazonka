{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
  ( Eac3AtmosSurroundExMode
    ( Eac3AtmosSurroundExMode'
    , Eac3AtmosSurroundExModeNotIndicated
    , Eac3AtmosSurroundExModeEnabled
    , Eac3AtmosSurroundExModeDisabled
    , fromEac3AtmosSurroundExMode
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
newtype Eac3AtmosSurroundExMode = Eac3AtmosSurroundExMode'{fromEac3AtmosSurroundExMode
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern Eac3AtmosSurroundExModeNotIndicated :: Eac3AtmosSurroundExMode
pattern Eac3AtmosSurroundExModeNotIndicated = Eac3AtmosSurroundExMode' "NOT_INDICATED"

pattern Eac3AtmosSurroundExModeEnabled :: Eac3AtmosSurroundExMode
pattern Eac3AtmosSurroundExModeEnabled = Eac3AtmosSurroundExMode' "ENABLED"

pattern Eac3AtmosSurroundExModeDisabled :: Eac3AtmosSurroundExMode
pattern Eac3AtmosSurroundExModeDisabled = Eac3AtmosSurroundExMode' "DISABLED"

{-# COMPLETE 
  Eac3AtmosSurroundExModeNotIndicated,

  Eac3AtmosSurroundExModeEnabled,

  Eac3AtmosSurroundExModeDisabled,
  Eac3AtmosSurroundExMode'
  #-}
