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
-- Module      : Amazonka.CloudFormation.Types.ChangeSetHooksStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ChangeSetHooksStatus
  ( ChangeSetHooksStatus
      ( ..,
        ChangeSetHooksStatus_PLANNED,
        ChangeSetHooksStatus_PLANNING,
        ChangeSetHooksStatus_UNAVAILABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeSetHooksStatus = ChangeSetHooksStatus'
  { fromChangeSetHooksStatus ::
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

pattern ChangeSetHooksStatus_PLANNED :: ChangeSetHooksStatus
pattern ChangeSetHooksStatus_PLANNED = ChangeSetHooksStatus' "PLANNED"

pattern ChangeSetHooksStatus_PLANNING :: ChangeSetHooksStatus
pattern ChangeSetHooksStatus_PLANNING = ChangeSetHooksStatus' "PLANNING"

pattern ChangeSetHooksStatus_UNAVAILABLE :: ChangeSetHooksStatus
pattern ChangeSetHooksStatus_UNAVAILABLE = ChangeSetHooksStatus' "UNAVAILABLE"

{-# COMPLETE
  ChangeSetHooksStatus_PLANNED,
  ChangeSetHooksStatus_PLANNING,
  ChangeSetHooksStatus_UNAVAILABLE,
  ChangeSetHooksStatus'
  #-}
