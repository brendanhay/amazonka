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
-- Module      : Amazonka.EC2.Types.VolumeModificationState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VolumeModificationState
  ( VolumeModificationState
      ( ..,
        VolumeModificationState_Completed,
        VolumeModificationState_Failed,
        VolumeModificationState_Modifying,
        VolumeModificationState_Optimizing
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype VolumeModificationState = VolumeModificationState'
  { fromVolumeModificationState ::
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

pattern VolumeModificationState_Completed :: VolumeModificationState
pattern VolumeModificationState_Completed = VolumeModificationState' "completed"

pattern VolumeModificationState_Failed :: VolumeModificationState
pattern VolumeModificationState_Failed = VolumeModificationState' "failed"

pattern VolumeModificationState_Modifying :: VolumeModificationState
pattern VolumeModificationState_Modifying = VolumeModificationState' "modifying"

pattern VolumeModificationState_Optimizing :: VolumeModificationState
pattern VolumeModificationState_Optimizing = VolumeModificationState' "optimizing"

{-# COMPLETE
  VolumeModificationState_Completed,
  VolumeModificationState_Failed,
  VolumeModificationState_Modifying,
  VolumeModificationState_Optimizing,
  VolumeModificationState'
  #-}
