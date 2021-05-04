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
-- Module      : Network.AWS.AppStream.Types.ImageState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageState
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
