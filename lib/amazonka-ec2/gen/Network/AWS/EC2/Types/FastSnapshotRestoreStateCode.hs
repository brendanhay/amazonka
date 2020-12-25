{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
  ( FastSnapshotRestoreStateCode
      ( FastSnapshotRestoreStateCode',
        FastSnapshotRestoreStateCodeEnabling,
        FastSnapshotRestoreStateCodeOptimizing,
        FastSnapshotRestoreStateCodeEnabled,
        FastSnapshotRestoreStateCodeDisabling,
        FastSnapshotRestoreStateCodeDisabled,
        fromFastSnapshotRestoreStateCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype FastSnapshotRestoreStateCode = FastSnapshotRestoreStateCode'
  { fromFastSnapshotRestoreStateCode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern FastSnapshotRestoreStateCodeEnabling :: FastSnapshotRestoreStateCode
pattern FastSnapshotRestoreStateCodeEnabling = FastSnapshotRestoreStateCode' "enabling"

pattern FastSnapshotRestoreStateCodeOptimizing :: FastSnapshotRestoreStateCode
pattern FastSnapshotRestoreStateCodeOptimizing = FastSnapshotRestoreStateCode' "optimizing"

pattern FastSnapshotRestoreStateCodeEnabled :: FastSnapshotRestoreStateCode
pattern FastSnapshotRestoreStateCodeEnabled = FastSnapshotRestoreStateCode' "enabled"

pattern FastSnapshotRestoreStateCodeDisabling :: FastSnapshotRestoreStateCode
pattern FastSnapshotRestoreStateCodeDisabling = FastSnapshotRestoreStateCode' "disabling"

pattern FastSnapshotRestoreStateCodeDisabled :: FastSnapshotRestoreStateCode
pattern FastSnapshotRestoreStateCodeDisabled = FastSnapshotRestoreStateCode' "disabled"

{-# COMPLETE
  FastSnapshotRestoreStateCodeEnabling,
  FastSnapshotRestoreStateCodeOptimizing,
  FastSnapshotRestoreStateCodeEnabled,
  FastSnapshotRestoreStateCodeDisabling,
  FastSnapshotRestoreStateCodeDisabled,
  FastSnapshotRestoreStateCode'
  #-}
