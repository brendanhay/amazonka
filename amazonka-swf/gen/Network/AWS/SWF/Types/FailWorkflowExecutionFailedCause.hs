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
-- Module      : Network.AWS.SWF.Types.FailWorkflowExecutionFailedCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.FailWorkflowExecutionFailedCause
  ( FailWorkflowExecutionFailedCause
      ( ..,
        FailWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
        FailWorkflowExecutionFailedCause_UNHANDLED_DECISION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype FailWorkflowExecutionFailedCause = FailWorkflowExecutionFailedCause'
  { fromFailWorkflowExecutionFailedCause ::
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

pattern FailWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED :: FailWorkflowExecutionFailedCause
pattern FailWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED = FailWorkflowExecutionFailedCause' "OPERATION_NOT_PERMITTED"

pattern FailWorkflowExecutionFailedCause_UNHANDLED_DECISION :: FailWorkflowExecutionFailedCause
pattern FailWorkflowExecutionFailedCause_UNHANDLED_DECISION = FailWorkflowExecutionFailedCause' "UNHANDLED_DECISION"

{-# COMPLETE
  FailWorkflowExecutionFailedCause_OPERATION_NOT_PERMITTED,
  FailWorkflowExecutionFailedCause_UNHANDLED_DECISION,
  FailWorkflowExecutionFailedCause'
  #-}
