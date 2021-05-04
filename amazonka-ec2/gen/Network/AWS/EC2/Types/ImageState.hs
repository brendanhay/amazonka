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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ImageState = ImageState'
  { fromImageState ::
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
