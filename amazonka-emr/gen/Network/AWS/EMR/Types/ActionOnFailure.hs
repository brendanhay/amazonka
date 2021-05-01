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

import qualified Network.AWS.Prelude as Prelude

newtype ActionOnFailure = ActionOnFailure'
  { fromActionOnFailure ::
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
