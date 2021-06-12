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
-- Module      : Network.AWS.SageMaker.Types.ImageVersionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersionStatus
  ( ImageVersionStatus
      ( ..,
        ImageVersionStatus_CREATED,
        ImageVersionStatus_CREATE_FAILED,
        ImageVersionStatus_CREATING,
        ImageVersionStatus_DELETE_FAILED,
        ImageVersionStatus_DELETING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ImageVersionStatus = ImageVersionStatus'
  { fromImageVersionStatus ::
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

pattern ImageVersionStatus_CREATED :: ImageVersionStatus
pattern ImageVersionStatus_CREATED = ImageVersionStatus' "CREATED"

pattern ImageVersionStatus_CREATE_FAILED :: ImageVersionStatus
pattern ImageVersionStatus_CREATE_FAILED = ImageVersionStatus' "CREATE_FAILED"

pattern ImageVersionStatus_CREATING :: ImageVersionStatus
pattern ImageVersionStatus_CREATING = ImageVersionStatus' "CREATING"

pattern ImageVersionStatus_DELETE_FAILED :: ImageVersionStatus
pattern ImageVersionStatus_DELETE_FAILED = ImageVersionStatus' "DELETE_FAILED"

pattern ImageVersionStatus_DELETING :: ImageVersionStatus
pattern ImageVersionStatus_DELETING = ImageVersionStatus' "DELETING"

{-# COMPLETE
  ImageVersionStatus_CREATED,
  ImageVersionStatus_CREATE_FAILED,
  ImageVersionStatus_CREATING,
  ImageVersionStatus_DELETE_FAILED,
  ImageVersionStatus_DELETING,
  ImageVersionStatus'
  #-}
