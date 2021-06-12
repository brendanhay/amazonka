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
-- Module      : Network.AWS.EMR.Types.ActionOnFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ActionOnFailure
  ( ActionOnFailure
      ( ..,
        ActionOnFailure_CANCEL_AND_WAIT,
        ActionOnFailure_CONTINUE,
        ActionOnFailure_TERMINATE_CLUSTER,
        ActionOnFailure_TERMINATE_JOB_FLOW
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ActionOnFailure = ActionOnFailure'
  { fromActionOnFailure ::
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
