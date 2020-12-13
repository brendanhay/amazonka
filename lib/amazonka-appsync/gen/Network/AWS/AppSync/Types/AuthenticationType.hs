{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        APIKey,
        AWSIAM,
        AmazonCognitoUserPools,
        OpenidConnect
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

pattern APIKey :: AuthenticationType
pattern APIKey = AuthenticationType' "API_KEY"

pattern AWSIAM :: AuthenticationType
pattern AWSIAM = AuthenticationType' "AWS_IAM"

pattern AmazonCognitoUserPools :: AuthenticationType
pattern AmazonCognitoUserPools = AuthenticationType' "AMAZON_COGNITO_USER_POOLS"

pattern OpenidConnect :: AuthenticationType
pattern OpenidConnect = AuthenticationType' "OPENID_CONNECT"

{-# COMPLETE
  APIKey,
  AWSIAM,
  AmazonCognitoUserPools,
  OpenidConnect,
  AuthenticationType'
  #-}
