-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackSetOperationAction
  ( StackSetOperationAction
      ( StackSetOperationAction',
        Create,
        Delete,
        DetectDrift,
        Update
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StackSetOperationAction = StackSetOperationAction' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Create :: StackSetOperationAction
pattern Create = StackSetOperationAction' "CREATE"

pattern Delete :: StackSetOperationAction
pattern Delete = StackSetOperationAction' "DELETE"

pattern DetectDrift :: StackSetOperationAction
pattern DetectDrift = StackSetOperationAction' "DETECT_DRIFT"

pattern Update :: StackSetOperationAction
pattern Update = StackSetOperationAction' "UPDATE"

{-# COMPLETE
  Create,
  Delete,
  DetectDrift,
  Update,
  StackSetOperationAction'
  #-}
