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
-- Module      : Network.AWS.SageMaker.Types.ImageStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageStatus
  ( ImageStatus
      ( ..,
        ImageStatus_CREATED,
        ImageStatus_CREATE_FAILED,
        ImageStatus_CREATING,
        ImageStatus_DELETE_FAILED,
        ImageStatus_DELETING,
        ImageStatus_UPDATE_FAILED,
        ImageStatus_UPDATING
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ImageStatus = ImageStatus'
  { fromImageStatus ::
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

pattern ImageStatus_CREATED :: ImageStatus
pattern ImageStatus_CREATED = ImageStatus' "CREATED"

pattern ImageStatus_CREATE_FAILED :: ImageStatus
pattern ImageStatus_CREATE_FAILED = ImageStatus' "CREATE_FAILED"

pattern ImageStatus_CREATING :: ImageStatus
pattern ImageStatus_CREATING = ImageStatus' "CREATING"

pattern ImageStatus_DELETE_FAILED :: ImageStatus
pattern ImageStatus_DELETE_FAILED = ImageStatus' "DELETE_FAILED"

pattern ImageStatus_DELETING :: ImageStatus
pattern ImageStatus_DELETING = ImageStatus' "DELETING"

pattern ImageStatus_UPDATE_FAILED :: ImageStatus
pattern ImageStatus_UPDATE_FAILED = ImageStatus' "UPDATE_FAILED"

pattern ImageStatus_UPDATING :: ImageStatus
pattern ImageStatus_UPDATING = ImageStatus' "UPDATING"

{-# COMPLETE
  ImageStatus_CREATED,
  ImageStatus_CREATE_FAILED,
  ImageStatus_CREATING,
  ImageStatus_DELETE_FAILED,
  ImageStatus_DELETING,
  ImageStatus_UPDATE_FAILED,
  ImageStatus_UPDATING,
  ImageStatus'
  #-}
