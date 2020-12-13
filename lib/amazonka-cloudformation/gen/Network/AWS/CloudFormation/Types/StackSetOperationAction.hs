{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        SSOACreate,
        SSOAUpdate,
        SSOADelete,
        SSOADetectDrift
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

pattern SSOACreate :: StackSetOperationAction
pattern SSOACreate = StackSetOperationAction' "CREATE"

pattern SSOAUpdate :: StackSetOperationAction
pattern SSOAUpdate = StackSetOperationAction' "UPDATE"

pattern SSOADelete :: StackSetOperationAction
pattern SSOADelete = StackSetOperationAction' "DELETE"

pattern SSOADetectDrift :: StackSetOperationAction
pattern SSOADetectDrift = StackSetOperationAction' "DETECT_DRIFT"

{-# COMPLETE
  SSOACreate,
  SSOAUpdate,
  SSOADelete,
  SSOADetectDrift,
  StackSetOperationAction'
  #-}
