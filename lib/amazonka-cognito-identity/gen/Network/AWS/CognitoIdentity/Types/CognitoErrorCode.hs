{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.CognitoErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.CognitoErrorCode
  ( CognitoErrorCode
      ( CognitoErrorCode',
        AccessDenied,
        InternalServerError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CognitoErrorCode = CognitoErrorCode' Lude.Text
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

pattern AccessDenied :: CognitoErrorCode
pattern AccessDenied = CognitoErrorCode' "AccessDenied"

pattern InternalServerError :: CognitoErrorCode
pattern InternalServerError = CognitoErrorCode' "InternalServerError"

{-# COMPLETE
  AccessDenied,
  InternalServerError,
  CognitoErrorCode'
  #-}
