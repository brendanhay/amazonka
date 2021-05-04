{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype FastSnapshotRestoreStateCode = FastSnapshotRestoreStateCode'
  { fromFastSnapshotRestoreStateCode ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
