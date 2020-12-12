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
        Failed,
        InProgress,
        Pending,
        ReadyForValidation,
        Succeeded
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

pattern Failed :: ValidationStatus
pattern Failed = ValidationStatus' "FAILED"

pattern InProgress :: ValidationStatus
pattern InProgress = ValidationStatus' "IN_PROGRESS"

pattern Pending :: ValidationStatus
pattern Pending = ValidationStatus' "PENDING"

pattern ReadyForValidation :: ValidationStatus
pattern ReadyForValidation = ValidationStatus' "READY_FOR_VALIDATION"

pattern Succeeded :: ValidationStatus
pattern Succeeded = ValidationStatus' "SUCCEEDED"

{-# COMPLETE
  Failed,
  InProgress,
  Pending,
  ReadyForValidation,
  Succeeded,
  ValidationStatus'
  #-}
