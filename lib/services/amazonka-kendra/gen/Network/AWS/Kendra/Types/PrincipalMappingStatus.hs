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
-- Module      : Network.AWS.Kendra.Types.PrincipalMappingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.PrincipalMappingStatus
  ( PrincipalMappingStatus
      ( ..,
        PrincipalMappingStatus_DELETED,
        PrincipalMappingStatus_DELETING,
        PrincipalMappingStatus_FAILED,
        PrincipalMappingStatus_PROCESSING,
        PrincipalMappingStatus_SUCCEEDED
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype PrincipalMappingStatus = PrincipalMappingStatus'
  { fromPrincipalMappingStatus ::
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

pattern PrincipalMappingStatus_DELETED :: PrincipalMappingStatus
pattern PrincipalMappingStatus_DELETED = PrincipalMappingStatus' "DELETED"

pattern PrincipalMappingStatus_DELETING :: PrincipalMappingStatus
pattern PrincipalMappingStatus_DELETING = PrincipalMappingStatus' "DELETING"

pattern PrincipalMappingStatus_FAILED :: PrincipalMappingStatus
pattern PrincipalMappingStatus_FAILED = PrincipalMappingStatus' "FAILED"

pattern PrincipalMappingStatus_PROCESSING :: PrincipalMappingStatus
pattern PrincipalMappingStatus_PROCESSING = PrincipalMappingStatus' "PROCESSING"

pattern PrincipalMappingStatus_SUCCEEDED :: PrincipalMappingStatus
pattern PrincipalMappingStatus_SUCCEEDED = PrincipalMappingStatus' "SUCCEEDED"

{-# COMPLETE
  PrincipalMappingStatus_DELETED,
  PrincipalMappingStatus_DELETING,
  PrincipalMappingStatus_FAILED,
  PrincipalMappingStatus_PROCESSING,
  PrincipalMappingStatus_SUCCEEDED,
  PrincipalMappingStatus'
  #-}
