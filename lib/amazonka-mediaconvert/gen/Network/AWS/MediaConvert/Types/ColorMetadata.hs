{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ColorMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ColorMetadata
  ( ColorMetadata
    ( ColorMetadata'
    , ColorMetadataIgnore
    , ColorMetadataInsert
    , fromColorMetadata
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Choose Insert (INSERT) for this setting to include color metadata in this output. Choose Ignore (IGNORE) to exclude color metadata from this output. If you don't specify a value, the service sets this to Insert by default.
newtype ColorMetadata = ColorMetadata'{fromColorMetadata ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern ColorMetadataIgnore :: ColorMetadata
pattern ColorMetadataIgnore = ColorMetadata' "IGNORE"

pattern ColorMetadataInsert :: ColorMetadata
pattern ColorMetadataInsert = ColorMetadata' "INSERT"

{-# COMPLETE 
  ColorMetadataIgnore,

  ColorMetadataInsert,
  ColorMetadata'
  #-}
