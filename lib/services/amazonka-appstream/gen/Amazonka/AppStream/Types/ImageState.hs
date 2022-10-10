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
-- Module      : Amazonka.AppStream.Types.ImageState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.ImageState
  ( ImageState
      ( ..,
        ImageState_AVAILABLE,
        ImageState_COPYING,
        ImageState_CREATING,
        ImageState_DELETING,
        ImageState_FAILED,
        ImageState_IMPORTING,
        ImageState_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ImageState = ImageState'
  { fromImageState ::
      Core.Text
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

pattern ImageState_AVAILABLE :: ImageState
pattern ImageState_AVAILABLE = ImageState' "AVAILABLE"

pattern ImageState_COPYING :: ImageState
pattern ImageState_COPYING = ImageState' "COPYING"

pattern ImageState_CREATING :: ImageState
pattern ImageState_CREATING = ImageState' "CREATING"

pattern ImageState_DELETING :: ImageState
pattern ImageState_DELETING = ImageState' "DELETING"

pattern ImageState_FAILED :: ImageState
pattern ImageState_FAILED = ImageState' "FAILED"

pattern ImageState_IMPORTING :: ImageState
pattern ImageState_IMPORTING = ImageState' "IMPORTING"

pattern ImageState_PENDING :: ImageState
pattern ImageState_PENDING = ImageState' "PENDING"

{-# COMPLETE
  ImageState_AVAILABLE,
  ImageState_COPYING,
  ImageState_CREATING,
  ImageState_DELETING,
  ImageState_FAILED,
  ImageState_IMPORTING,
  ImageState_PENDING,
  ImageState'
  #-}
