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
-- Module      : Amazonka.Kendra.Types.PrincipalMappingStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.PrincipalMappingStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype PrincipalMappingStatus = PrincipalMappingStatus'
  { fromPrincipalMappingStatus ::
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
