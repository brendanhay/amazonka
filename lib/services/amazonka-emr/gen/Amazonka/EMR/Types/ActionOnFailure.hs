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
-- Module      : Amazonka.EMR.Types.ActionOnFailure
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ActionOnFailure
  ( ActionOnFailure
      ( ..,
        ActionOnFailure_CANCEL_AND_WAIT,
        ActionOnFailure_CONTINUE,
        ActionOnFailure_TERMINATE_CLUSTER,
        ActionOnFailure_TERMINATE_JOB_FLOW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ActionOnFailure = ActionOnFailure'
  { fromActionOnFailure ::
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

pattern ActionOnFailure_CANCEL_AND_WAIT :: ActionOnFailure
pattern ActionOnFailure_CANCEL_AND_WAIT = ActionOnFailure' "CANCEL_AND_WAIT"

pattern ActionOnFailure_CONTINUE :: ActionOnFailure
pattern ActionOnFailure_CONTINUE = ActionOnFailure' "CONTINUE"

pattern ActionOnFailure_TERMINATE_CLUSTER :: ActionOnFailure
pattern ActionOnFailure_TERMINATE_CLUSTER = ActionOnFailure' "TERMINATE_CLUSTER"

pattern ActionOnFailure_TERMINATE_JOB_FLOW :: ActionOnFailure
pattern ActionOnFailure_TERMINATE_JOB_FLOW = ActionOnFailure' "TERMINATE_JOB_FLOW"

{-# COMPLETE
  ActionOnFailure_CANCEL_AND_WAIT,
  ActionOnFailure_CONTINUE,
  ActionOnFailure_TERMINATE_CLUSTER,
  ActionOnFailure_TERMINATE_JOB_FLOW,
  ActionOnFailure'
  #-}
