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

import qualified Network.AWS.Prelude as Prelude

newtype PatchDeploymentStatus = PatchDeploymentStatus'
  { fromPatchDeploymentStatus ::
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
