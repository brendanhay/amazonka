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
        VSPending,
        VSSuccess,
        VSFailed,
        VSTemporaryFailure,
        VSNotStarted
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

pattern VSPending :: VerificationStatus
pattern VSPending = VerificationStatus' "Pending"

pattern VSSuccess :: VerificationStatus
pattern VSSuccess = VerificationStatus' "Success"

pattern VSFailed :: VerificationStatus
pattern VSFailed = VerificationStatus' "Failed"

pattern VSTemporaryFailure :: VerificationStatus
pattern VSTemporaryFailure = VerificationStatus' "TemporaryFailure"

pattern VSNotStarted :: VerificationStatus
pattern VSNotStarted = VerificationStatus' "NotStarted"

{-# COMPLETE
  VSPending,
  VSSuccess,
  VSFailed,
  VSTemporaryFailure,
  VSNotStarted,
  VerificationStatus'
  #-}
