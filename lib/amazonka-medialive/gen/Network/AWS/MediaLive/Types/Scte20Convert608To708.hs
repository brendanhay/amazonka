{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte20Convert608To708
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte20Convert608To708
  ( Scte20Convert608To708
    ( Scte20Convert608To708'
    , Scte20Convert608To708Disabled
    , Scte20Convert608To708Upconvert
    , fromScte20Convert608To708
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Scte20 Convert608 To708
newtype Scte20Convert608To708 = Scte20Convert608To708'{fromScte20Convert608To708
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern Scte20Convert608To708Disabled :: Scte20Convert608To708
pattern Scte20Convert608To708Disabled = Scte20Convert608To708' "DISABLED"

pattern Scte20Convert608To708Upconvert :: Scte20Convert608To708
pattern Scte20Convert608To708Upconvert = Scte20Convert608To708' "UPCONVERT"

{-# COMPLETE 
  Scte20Convert608To708Disabled,

  Scte20Convert608To708Upconvert,
  Scte20Convert608To708'
  #-}
