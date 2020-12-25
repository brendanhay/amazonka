{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.VerificationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.VerificationStatus
  ( VerificationStatus
      ( VerificationStatus',
        VerificationStatusPending,
        VerificationStatusSuccess,
        VerificationStatusFailed,
        VerificationStatusTemporaryFailure,
        VerificationStatusNotStarted,
        fromVerificationStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype VerificationStatus = VerificationStatus'
  { fromVerificationStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern VerificationStatusPending :: VerificationStatus
pattern VerificationStatusPending = VerificationStatus' "Pending"

pattern VerificationStatusSuccess :: VerificationStatus
pattern VerificationStatusSuccess = VerificationStatus' "Success"

pattern VerificationStatusFailed :: VerificationStatus
pattern VerificationStatusFailed = VerificationStatus' "Failed"

pattern VerificationStatusTemporaryFailure :: VerificationStatus
pattern VerificationStatusTemporaryFailure = VerificationStatus' "TemporaryFailure"

pattern VerificationStatusNotStarted :: VerificationStatus
pattern VerificationStatusNotStarted = VerificationStatus' "NotStarted"

{-# COMPLETE
  VerificationStatusPending,
  VerificationStatusSuccess,
  VerificationStatusFailed,
  VerificationStatusTemporaryFailure,
  VerificationStatusNotStarted,
  VerificationStatus'
  #-}
