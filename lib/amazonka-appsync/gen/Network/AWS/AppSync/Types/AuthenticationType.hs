-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AuthenticationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AuthenticationType
  ( AuthenticationType
      ( AuthenticationType',
        ATAPIKey,
        ATAWSIAM,
        ATAmazonCognitoUserPools,
        ATOpenidConnect
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AuthenticationType = AuthenticationType' Lude.Text
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

pattern ATAPIKey :: AuthenticationType
pattern ATAPIKey = AuthenticationType' "API_KEY"

pattern ATAWSIAM :: AuthenticationType
pattern ATAWSIAM = AuthenticationType' "AWS_IAM"

pattern ATAmazonCognitoUserPools :: AuthenticationType
pattern ATAmazonCognitoUserPools = AuthenticationType' "AMAZON_COGNITO_USER_POOLS"

pattern ATOpenidConnect :: AuthenticationType
pattern ATOpenidConnect = AuthenticationType' "OPENID_CONNECT"

{-# COMPLETE
  ATAPIKey,
  ATAWSIAM,
  ATAmazonCognitoUserPools,
  ATOpenidConnect,
  AuthenticationType'
  #-}
