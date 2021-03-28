{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafSegmentControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.CmafSegmentControl
  ( CmafSegmentControl
    ( CmafSegmentControl'
    , CmafSegmentControlSingleFile
    , CmafSegmentControlSegmentedFiles
    , fromCmafSegmentControl
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
newtype CmafSegmentControl = CmafSegmentControl'{fromCmafSegmentControl
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern CmafSegmentControlSingleFile :: CmafSegmentControl
pattern CmafSegmentControlSingleFile = CmafSegmentControl' "SINGLE_FILE"

pattern CmafSegmentControlSegmentedFiles :: CmafSegmentControl
pattern CmafSegmentControlSegmentedFiles = CmafSegmentControl' "SEGMENTED_FILES"

{-# COMPLETE 
  CmafSegmentControlSingleFile,

  CmafSegmentControlSegmentedFiles,
  CmafSegmentControl'
  #-}
