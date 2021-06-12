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
-- Module      : Network.AWS.SSM.Types.PatchDeploymentStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchDeploymentStatus
  ( PatchDeploymentStatus
      ( ..,
        PatchDeploymentStatus_APPROVED,
        PatchDeploymentStatus_EXPLICIT_APPROVED,
        PatchDeploymentStatus_EXPLICIT_REJECTED,
        PatchDeploymentStatus_PENDING_APPROVAL
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype PatchDeploymentStatus = PatchDeploymentStatus'
  { fromPatchDeploymentStatus ::
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

pattern PatchDeploymentStatus_APPROVED :: PatchDeploymentStatus
pattern PatchDeploymentStatus_APPROVED = PatchDeploymentStatus' "APPROVED"

pattern PatchDeploymentStatus_EXPLICIT_APPROVED :: PatchDeploymentStatus
pattern PatchDeploymentStatus_EXPLICIT_APPROVED = PatchDeploymentStatus' "EXPLICIT_APPROVED"

pattern PatchDeploymentStatus_EXPLICIT_REJECTED :: PatchDeploymentStatus
pattern PatchDeploymentStatus_EXPLICIT_REJECTED = PatchDeploymentStatus' "EXPLICIT_REJECTED"

pattern PatchDeploymentStatus_PENDING_APPROVAL :: PatchDeploymentStatus
pattern PatchDeploymentStatus_PENDING_APPROVAL = PatchDeploymentStatus' "PENDING_APPROVAL"

{-# COMPLETE
  PatchDeploymentStatus_APPROVED,
  PatchDeploymentStatus_EXPLICIT_APPROVED,
  PatchDeploymentStatus_EXPLICIT_REJECTED,
  PatchDeploymentStatus_PENDING_APPROVAL,
  PatchDeploymentStatus'
  #-}
