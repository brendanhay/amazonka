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
-- Module      : Amazonka.CloudFormation.Types.StackSetOperationAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetOperationAction
  ( StackSetOperationAction
      ( ..,
        StackSetOperationAction_CREATE,
        StackSetOperationAction_DELETE,
        StackSetOperationAction_DETECT_DRIFT,
        StackSetOperationAction_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StackSetOperationAction = StackSetOperationAction'
  { fromStackSetOperationAction ::
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

pattern StackSetOperationAction_CREATE :: StackSetOperationAction
pattern StackSetOperationAction_CREATE = StackSetOperationAction' "CREATE"

pattern StackSetOperationAction_DELETE :: StackSetOperationAction
pattern StackSetOperationAction_DELETE = StackSetOperationAction' "DELETE"

pattern StackSetOperationAction_DETECT_DRIFT :: StackSetOperationAction
pattern StackSetOperationAction_DETECT_DRIFT = StackSetOperationAction' "DETECT_DRIFT"

pattern StackSetOperationAction_UPDATE :: StackSetOperationAction
pattern StackSetOperationAction_UPDATE = StackSetOperationAction' "UPDATE"

{-# COMPLETE
  StackSetOperationAction_CREATE,
  StackSetOperationAction_DELETE,
  StackSetOperationAction_DETECT_DRIFT,
  StackSetOperationAction_UPDATE,
  StackSetOperationAction'
  #-}
