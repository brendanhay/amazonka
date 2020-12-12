{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.FailureReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.FailureReason
  ( FailureReason
      ( FailureReason',
        Other,
        RequestTimedOut,
        UnsupportedAlgorithm
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FailureReason = FailureReason' Lude.Text
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

pattern Other :: FailureReason
pattern Other = FailureReason' "OTHER"

pattern RequestTimedOut :: FailureReason
pattern RequestTimedOut = FailureReason' "REQUEST_TIMED_OUT"

pattern UnsupportedAlgorithm :: FailureReason
pattern UnsupportedAlgorithm = FailureReason' "UNSUPPORTED_ALGORITHM"

{-# COMPLETE
  Other,
  RequestTimedOut,
  UnsupportedAlgorithm,
  FailureReason'
  #-}
