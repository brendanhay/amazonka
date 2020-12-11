-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ChallengeResponse
  ( ChallengeResponse
      ( ChallengeResponse',
        CFailure,
        CSuccess
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ChallengeResponse = ChallengeResponse' Lude.Text
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

pattern CFailure :: ChallengeResponse
pattern CFailure = ChallengeResponse' "Failure"

pattern CSuccess :: ChallengeResponse
pattern CSuccess = ChallengeResponse' "Success"

{-# COMPLETE
  CFailure,
  CSuccess,
  ChallengeResponse'
  #-}
