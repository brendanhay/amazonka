{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MovPaddingControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MovPaddingControl
  ( MovPaddingControl
    ( MovPaddingControl'
    , MovPaddingControlOmneon
    , MovPaddingControlNone
    , fromMovPaddingControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | To make this output compatible with Omenon, keep the default value, OMNEON. Unless you need Omneon compatibility, set this value to NONE. When you keep the default value, OMNEON, MediaConvert increases the length of the edit list atom. This might cause file rejections when a recipient of the output file doesn't expct this extra padding.
newtype MovPaddingControl = MovPaddingControl'{fromMovPaddingControl
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern MovPaddingControlOmneon :: MovPaddingControl
pattern MovPaddingControlOmneon = MovPaddingControl' "OMNEON"

pattern MovPaddingControlNone :: MovPaddingControl
pattern MovPaddingControlNone = MovPaddingControl' "NONE"

{-# COMPLETE 
  MovPaddingControlOmneon,

  MovPaddingControlNone,
  MovPaddingControl'
  #-}
