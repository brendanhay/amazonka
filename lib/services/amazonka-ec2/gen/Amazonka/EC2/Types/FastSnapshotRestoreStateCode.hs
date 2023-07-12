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
-- Module      : Amazonka.EC2.Types.FastSnapshotRestoreStateCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.FastSnapshotRestoreStateCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype FastSnapshotRestoreStateCode = FastSnapshotRestoreStateCode'
  { fromFastSnapshotRestoreStateCode ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
