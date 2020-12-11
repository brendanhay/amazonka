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
        VSFailed,
        VSNotStarted,
        VSPending,
        VSSuccess,
        VSTemporaryFailure
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype VerificationStatus = VerificationStatus' Lude.Text
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

pattern VSFailed :: VerificationStatus
pattern VSFailed = VerificationStatus' "Failed"

pattern VSNotStarted :: VerificationStatus
pattern VSNotStarted = VerificationStatus' "NotStarted"

pattern VSPending :: VerificationStatus
pattern VSPending = VerificationStatus' "Pending"

pattern VSSuccess :: VerificationStatus
pattern VSSuccess = VerificationStatus' "Success"

pattern VSTemporaryFailure :: VerificationStatus
pattern VSTemporaryFailure = VerificationStatus' "TemporaryFailure"

{-# COMPLETE
  VSFailed,
  VSNotStarted,
  VSPending,
  VSSuccess,
  VSTemporaryFailure,
  VerificationStatus'
  #-}
