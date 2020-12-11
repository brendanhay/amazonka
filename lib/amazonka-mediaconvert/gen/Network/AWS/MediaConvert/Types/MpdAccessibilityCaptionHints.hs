-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
  ( MpdAccessibilityCaptionHints
      ( MpdAccessibilityCaptionHints',
        MACHExclude,
        MACHInclude
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | <Accessibility>elements for embedded 608 captions. This markup isn't generally required, but some video players require it to discover and play embedded 608 captions. Keep the default value, Exclude (EXCLUDE), to leave these elements out. When you enable this setting, this is the markup that MediaConvert includes in your manifest: <Accessibility>
newtype MpdAccessibilityCaptionHints = MpdAccessibilityCaptionHints' Lude.Text
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

pattern MACHExclude :: MpdAccessibilityCaptionHints
pattern MACHExclude = MpdAccessibilityCaptionHints' "EXCLUDE"

pattern MACHInclude :: MpdAccessibilityCaptionHints
pattern MACHInclude = MpdAccessibilityCaptionHints' "INCLUDE"

{-# COMPLETE
  MACHExclude,
  MACHInclude,
  MpdAccessibilityCaptionHints'
  #-}
