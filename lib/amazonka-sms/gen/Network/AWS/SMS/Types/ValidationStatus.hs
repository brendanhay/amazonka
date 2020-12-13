{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ValidationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ValidationStatus
  ( ValidationStatus
      ( ValidationStatus',
        ReadyForValidation,
        Pending,
        InProgress,
        Succeeded,
        Failed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ValidationStatus = ValidationStatus' Lude.Text
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

pattern ReadyForValidation :: ValidationStatus
pattern ReadyForValidation = ValidationStatus' "READY_FOR_VALIDATION"

pattern Pending :: ValidationStatus
pattern Pending = ValidationStatus' "PENDING"

pattern InProgress :: ValidationStatus
pattern InProgress = ValidationStatus' "IN_PROGRESS"

pattern Succeeded :: ValidationStatus
pattern Succeeded = ValidationStatus' "SUCCEEDED"

pattern Failed :: ValidationStatus
pattern Failed = ValidationStatus' "FAILED"

{-# COMPLETE
  ReadyForValidation,
  Pending,
  InProgress,
  Succeeded,
  Failed,
  ValidationStatus'
  #-}
