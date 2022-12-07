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
-- Module      : Amazonka.CustomerProfiles.Types.IdentityResolutionJobStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.IdentityResolutionJobStatus
  ( IdentityResolutionJobStatus
      ( ..,
        IdentityResolutionJobStatus_COMPLETED,
        IdentityResolutionJobStatus_FAILED,
        IdentityResolutionJobStatus_FIND_MATCHING,
        IdentityResolutionJobStatus_MERGING,
        IdentityResolutionJobStatus_PARTIAL_SUCCESS,
        IdentityResolutionJobStatus_PENDING,
        IdentityResolutionJobStatus_PREPROCESSING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype IdentityResolutionJobStatus = IdentityResolutionJobStatus'
  { fromIdentityResolutionJobStatus ::
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

pattern IdentityResolutionJobStatus_COMPLETED :: IdentityResolutionJobStatus
pattern IdentityResolutionJobStatus_COMPLETED = IdentityResolutionJobStatus' "COMPLETED"

pattern IdentityResolutionJobStatus_FAILED :: IdentityResolutionJobStatus
pattern IdentityResolutionJobStatus_FAILED = IdentityResolutionJobStatus' "FAILED"

pattern IdentityResolutionJobStatus_FIND_MATCHING :: IdentityResolutionJobStatus
pattern IdentityResolutionJobStatus_FIND_MATCHING = IdentityResolutionJobStatus' "FIND_MATCHING"

pattern IdentityResolutionJobStatus_MERGING :: IdentityResolutionJobStatus
pattern IdentityResolutionJobStatus_MERGING = IdentityResolutionJobStatus' "MERGING"

pattern IdentityResolutionJobStatus_PARTIAL_SUCCESS :: IdentityResolutionJobStatus
pattern IdentityResolutionJobStatus_PARTIAL_SUCCESS = IdentityResolutionJobStatus' "PARTIAL_SUCCESS"

pattern IdentityResolutionJobStatus_PENDING :: IdentityResolutionJobStatus
pattern IdentityResolutionJobStatus_PENDING = IdentityResolutionJobStatus' "PENDING"

pattern IdentityResolutionJobStatus_PREPROCESSING :: IdentityResolutionJobStatus
pattern IdentityResolutionJobStatus_PREPROCESSING = IdentityResolutionJobStatus' "PREPROCESSING"

{-# COMPLETE
  IdentityResolutionJobStatus_COMPLETED,
  IdentityResolutionJobStatus_FAILED,
  IdentityResolutionJobStatus_FIND_MATCHING,
  IdentityResolutionJobStatus_MERGING,
  IdentityResolutionJobStatus_PARTIAL_SUCCESS,
  IdentityResolutionJobStatus_PENDING,
  IdentityResolutionJobStatus_PREPROCESSING,
  IdentityResolutionJobStatus'
  #-}
