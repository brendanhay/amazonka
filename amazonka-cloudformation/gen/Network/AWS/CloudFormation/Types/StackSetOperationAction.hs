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
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationAction
  ( StackSetOperationAction
      ( ..,
        StackSetOperationAction_CREATE,
        StackSetOperationAction_DELETE,
        StackSetOperationAction_DETECT_DRIFT,
        StackSetOperationAction_UPDATE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype StackSetOperationAction = StackSetOperationAction'
  { fromStackSetOperationAction ::
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
