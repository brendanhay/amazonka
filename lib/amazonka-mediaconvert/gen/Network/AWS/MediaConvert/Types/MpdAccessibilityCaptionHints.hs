{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
  ( MpdAccessibilityCaptionHints
    ( MpdAccessibilityCaptionHints'
    , MpdAccessibilityCaptionHintsInclude
    , MpdAccessibilityCaptionHintsExclude
    , fromMpdAccessibilityCaptionHints
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | <Accessibility>elements for embedded 608 captions. This markup isn't generally required, but some video players require it to discover and play embedded 608 captions. Keep the default value, Exclude (EXCLUDE), to leave these elements out. When you enable this setting, this is the markup that MediaConvert includes in your manifest: <Accessibility>
newtype MpdAccessibilityCaptionHints = MpdAccessibilityCaptionHints'{fromMpdAccessibilityCaptionHints
                                                                     :: Core.Text}
                                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                         Core.Generic)
                                         deriving newtype (Core.IsString, Core.Hashable,
                                                           Core.NFData, Core.ToJSONKey,
                                                           Core.FromJSONKey, Core.ToJSON,
                                                           Core.FromJSON, Core.ToXML, Core.FromXML,
                                                           Core.ToText, Core.FromText,
                                                           Core.ToByteString, Core.ToQuery,
                                                           Core.ToHeader)

pattern MpdAccessibilityCaptionHintsInclude :: MpdAccessibilityCaptionHints
pattern MpdAccessibilityCaptionHintsInclude = MpdAccessibilityCaptionHints' "INCLUDE"

pattern MpdAccessibilityCaptionHintsExclude :: MpdAccessibilityCaptionHints
pattern MpdAccessibilityCaptionHintsExclude = MpdAccessibilityCaptionHints' "EXCLUDE"

{-# COMPLETE 
  MpdAccessibilityCaptionHintsInclude,

  MpdAccessibilityCaptionHintsExclude,
  MpdAccessibilityCaptionHints'
  #-}
