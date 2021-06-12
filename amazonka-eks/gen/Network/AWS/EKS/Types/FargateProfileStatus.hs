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
-- Module      : Network.AWS.EKS.Types.FargateProfileStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.FargateProfileStatus
  ( FargateProfileStatus
      ( ..,
        FargateProfileStatus_ACTIVE,
        FargateProfileStatus_CREATE_FAILED,
        FargateProfileStatus_CREATING,
        FargateProfileStatus_DELETE_FAILED,
        FargateProfileStatus_DELETING
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype FargateProfileStatus = FargateProfileStatus'
  { fromFargateProfileStatus ::
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

pattern FargateProfileStatus_ACTIVE :: FargateProfileStatus
pattern FargateProfileStatus_ACTIVE = FargateProfileStatus' "ACTIVE"

pattern FargateProfileStatus_CREATE_FAILED :: FargateProfileStatus
pattern FargateProfileStatus_CREATE_FAILED = FargateProfileStatus' "CREATE_FAILED"

pattern FargateProfileStatus_CREATING :: FargateProfileStatus
pattern FargateProfileStatus_CREATING = FargateProfileStatus' "CREATING"

pattern FargateProfileStatus_DELETE_FAILED :: FargateProfileStatus
pattern FargateProfileStatus_DELETE_FAILED = FargateProfileStatus' "DELETE_FAILED"

pattern FargateProfileStatus_DELETING :: FargateProfileStatus
pattern FargateProfileStatus_DELETING = FargateProfileStatus' "DELETING"

{-# COMPLETE
  FargateProfileStatus_ACTIVE,
  FargateProfileStatus_CREATE_FAILED,
  FargateProfileStatus_CREATING,
  FargateProfileStatus_DELETE_FAILED,
  FargateProfileStatus_DELETING,
  FargateProfileStatus'
  #-}
