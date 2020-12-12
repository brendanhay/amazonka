{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoSegmentControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoSegmentControl
  ( DashIsoSegmentControl
      ( DashIsoSegmentControl',
        DISCSegmentedFiles,
        DISCSingleFile
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | When set to SINGLE_FILE, a single output file is generated, which is internally segmented using the Fragment Length and Segment Length. When set to SEGMENTED_FILES, separate segment files will be created.
newtype DashIsoSegmentControl = DashIsoSegmentControl' Lude.Text
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

pattern DISCSegmentedFiles :: DashIsoSegmentControl
pattern DISCSegmentedFiles = DashIsoSegmentControl' "SEGMENTED_FILES"

pattern DISCSingleFile :: DashIsoSegmentControl
pattern DISCSingleFile = DashIsoSegmentControl' "SINGLE_FILE"

{-# COMPLETE
  DISCSegmentedFiles,
  DISCSingleFile,
  DashIsoSegmentControl'
  #-}
