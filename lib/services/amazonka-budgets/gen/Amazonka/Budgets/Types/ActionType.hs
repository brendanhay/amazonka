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
-- Module      : Amazonka.Budgets.Types.ActionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.ActionType
  ( ActionType
      ( ..,
        ActionType_APPLY_IAM_POLICY,
        ActionType_APPLY_SCP_POLICY,
        ActionType_RUN_SSM_DOCUMENTS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionType = ActionType'
  { fromActionType ::
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

pattern ActionType_APPLY_IAM_POLICY :: ActionType
pattern ActionType_APPLY_IAM_POLICY = ActionType' "APPLY_IAM_POLICY"

pattern ActionType_APPLY_SCP_POLICY :: ActionType
pattern ActionType_APPLY_SCP_POLICY = ActionType' "APPLY_SCP_POLICY"

pattern ActionType_RUN_SSM_DOCUMENTS :: ActionType
pattern ActionType_RUN_SSM_DOCUMENTS = ActionType' "RUN_SSM_DOCUMENTS"

{-# COMPLETE
  ActionType_APPLY_IAM_POLICY,
  ActionType_APPLY_SCP_POLICY,
  ActionType_RUN_SSM_DOCUMENTS,
  ActionType'
  #-}
