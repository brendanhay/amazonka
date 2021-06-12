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
-- Module      : Network.AWS.EC2.Types.VolumeState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VolumeState
  ( VolumeState
      ( ..,
        VolumeState_Available,
        VolumeState_Creating,
        VolumeState_Deleted,
        VolumeState_Deleting,
        VolumeState_Error,
        VolumeState_In_use
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype VolumeState = VolumeState'
  { fromVolumeState ::
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

pattern VolumeState_Available :: VolumeState
pattern VolumeState_Available = VolumeState' "available"

pattern VolumeState_Creating :: VolumeState
pattern VolumeState_Creating = VolumeState' "creating"

pattern VolumeState_Deleted :: VolumeState
pattern VolumeState_Deleted = VolumeState' "deleted"

pattern VolumeState_Deleting :: VolumeState
pattern VolumeState_Deleting = VolumeState' "deleting"

pattern VolumeState_Error :: VolumeState
pattern VolumeState_Error = VolumeState' "error"

pattern VolumeState_In_use :: VolumeState
pattern VolumeState_In_use = VolumeState' "in-use"

{-# COMPLETE
  VolumeState_Available,
  VolumeState_Creating,
  VolumeState_Deleted,
  VolumeState_Deleting,
  VolumeState_Error,
  VolumeState_In_use,
  VolumeState'
  #-}
