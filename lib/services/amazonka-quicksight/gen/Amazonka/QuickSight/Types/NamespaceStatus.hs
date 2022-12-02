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
-- Module      : Amazonka.QuickSight.Types.NamespaceStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NamespaceStatus
  ( NamespaceStatus
      ( ..,
        NamespaceStatus_CREATED,
        NamespaceStatus_CREATING,
        NamespaceStatus_DELETING,
        NamespaceStatus_NON_RETRYABLE_FAILURE,
        NamespaceStatus_RETRYABLE_FAILURE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NamespaceStatus = NamespaceStatus'
  { fromNamespaceStatus ::
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

pattern NamespaceStatus_CREATED :: NamespaceStatus
pattern NamespaceStatus_CREATED = NamespaceStatus' "CREATED"

pattern NamespaceStatus_CREATING :: NamespaceStatus
pattern NamespaceStatus_CREATING = NamespaceStatus' "CREATING"

pattern NamespaceStatus_DELETING :: NamespaceStatus
pattern NamespaceStatus_DELETING = NamespaceStatus' "DELETING"

pattern NamespaceStatus_NON_RETRYABLE_FAILURE :: NamespaceStatus
pattern NamespaceStatus_NON_RETRYABLE_FAILURE = NamespaceStatus' "NON_RETRYABLE_FAILURE"

pattern NamespaceStatus_RETRYABLE_FAILURE :: NamespaceStatus
pattern NamespaceStatus_RETRYABLE_FAILURE = NamespaceStatus' "RETRYABLE_FAILURE"

{-# COMPLETE
  NamespaceStatus_CREATED,
  NamespaceStatus_CREATING,
  NamespaceStatus_DELETING,
  NamespaceStatus_NON_RETRYABLE_FAILURE,
  NamespaceStatus_RETRYABLE_FAILURE,
  NamespaceStatus'
  #-}
