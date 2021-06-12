{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.CmafSegmentControl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.CmafSegmentControl
  ( CmafSegmentControl
      ( ..,
        CmafSegmentControl_SEGMENTED_FILES,
        CmafSegmentControl_SINGLE_FILE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | When set to SINGLE_FILE, a single output file is generated, which is
-- internally segmented using the Fragment Length and Segment Length. When
-- set to SEGMENTED_FILES, separate segment files will be created.
newtype CmafSegmentControl = CmafSegmentControl'
  { fromCmafSegmentControl ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern CmafSegmentControl_SEGMENTED_FILES :: CmafSegmentControl
pattern CmafSegmentControl_SEGMENTED_FILES = CmafSegmentControl' "SEGMENTED_FILES"

pattern CmafSegmentControl_SINGLE_FILE :: CmafSegmentControl
pattern CmafSegmentControl_SINGLE_FILE = CmafSegmentControl' "SINGLE_FILE"

{-# COMPLETE
  CmafSegmentControl_SEGMENTED_FILES,
  CmafSegmentControl_SINGLE_FILE,
  CmafSegmentControl'
  #-}
