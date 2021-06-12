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
-- Module      : Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FastSnapshotRestoreStateCode
  ( FastSnapshotRestoreStateCode
      ( ..,
        FastSnapshotRestoreStateCode_Disabled,
        FastSnapshotRestoreStateCode_Disabling,
        FastSnapshotRestoreStateCode_Enabled,
        FastSnapshotRestoreStateCode_Enabling,
        FastSnapshotRestoreStateCode_Optimizing
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype FastSnapshotRestoreStateCode = FastSnapshotRestoreStateCode'
  { fromFastSnapshotRestoreStateCode ::
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

pattern FastSnapshotRestoreStateCode_Disabled :: FastSnapshotRestoreStateCode
pattern FastSnapshotRestoreStateCode_Disabled = FastSnapshotRestoreStateCode' "disabled"

pattern FastSnapshotRestoreStateCode_Disabling :: FastSnapshotRestoreStateCode
pattern FastSnapshotRestoreStateCode_Disabling = FastSnapshotRestoreStateCode' "disabling"

pattern FastSnapshotRestoreStateCode_Enabled :: FastSnapshotRestoreStateCode
pattern FastSnapshotRestoreStateCode_Enabled = FastSnapshotRestoreStateCode' "enabled"

pattern FastSnapshotRestoreStateCode_Enabling :: FastSnapshotRestoreStateCode
pattern FastSnapshotRestoreStateCode_Enabling = FastSnapshotRestoreStateCode' "enabling"

pattern FastSnapshotRestoreStateCode_Optimizing :: FastSnapshotRestoreStateCode
pattern FastSnapshotRestoreStateCode_Optimizing = FastSnapshotRestoreStateCode' "optimizing"

{-# COMPLETE
  FastSnapshotRestoreStateCode_Disabled,
  FastSnapshotRestoreStateCode_Disabling,
  FastSnapshotRestoreStateCode_Enabled,
  FastSnapshotRestoreStateCode_Enabling,
  FastSnapshotRestoreStateCode_Optimizing,
  FastSnapshotRestoreStateCode'
  #-}
