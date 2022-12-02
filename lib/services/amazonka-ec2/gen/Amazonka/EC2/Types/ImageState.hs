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
-- Module      : Amazonka.EC2.Types.ImageState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ImageState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ImageState = ImageState'
  { fromImageState ::
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
