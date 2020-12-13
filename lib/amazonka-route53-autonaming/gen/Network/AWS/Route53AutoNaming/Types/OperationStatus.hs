{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.OperationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.OperationStatus
  ( OperationStatus
      ( OperationStatus',
        Submitted,
        Pending,
        Success,
        Fail
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype OperationStatus = OperationStatus' Lude.Text
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

pattern Submitted :: OperationStatus
pattern Submitted = OperationStatus' "SUBMITTED"

pattern Pending :: OperationStatus
pattern Pending = OperationStatus' "PENDING"

pattern Success :: OperationStatus
pattern Success = OperationStatus' "SUCCESS"

pattern Fail :: OperationStatus
pattern Fail = OperationStatus' "FAIL"

{-# COMPLETE
  Submitted,
  Pending,
  Success,
  Fail,
  OperationStatus'
  #-}
