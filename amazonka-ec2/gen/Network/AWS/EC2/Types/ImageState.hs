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
-- Module      : Network.AWS.EC2.Types.ImageState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ImageState
  ( ImageState
      ( ..,
        ImageState_Available,
        ImageState_Deregistered,
        ImageState_Error,
        ImageState_Failed,
        ImageState_Invalid,
        ImageState_Pending,
        ImageState_Transient
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype ImageState = ImageState'
  { fromImageState ::
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

pattern ImageState_Available :: ImageState
pattern ImageState_Available = ImageState' "available"

pattern ImageState_Deregistered :: ImageState
pattern ImageState_Deregistered = ImageState' "deregistered"

pattern ImageState_Error :: ImageState
pattern ImageState_Error = ImageState' "error"

pattern ImageState_Failed :: ImageState
pattern ImageState_Failed = ImageState' "failed"

pattern ImageState_Invalid :: ImageState
pattern ImageState_Invalid = ImageState' "invalid"

pattern ImageState_Pending :: ImageState
pattern ImageState_Pending = ImageState' "pending"

pattern ImageState_Transient :: ImageState
pattern ImageState_Transient = ImageState' "transient"

{-# COMPLETE
  ImageState_Available,
  ImageState_Deregistered,
  ImageState_Error,
  ImageState_Failed,
  ImageState_Invalid,
  ImageState_Pending,
  ImageState_Transient,
  ImageState'
  #-}
