{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentity.Types.Product where

import Network.AWS.CognitoIdentity.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A provider representing an Amazon Cognito user pool and its client ID.
--
--
--
-- /See:/ 'cognitoIdentityProvider' smart constructor.
data CognitoIdentityProvider = CognitoIdentityProvider'
  { _cipClientId             :: !(Maybe Text)
  , _cipServerSideTokenCheck :: !(Maybe Bool)
  , _cipProviderName         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CognitoIdentityProvider' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cipClientId' - The client ID for the Amazon Cognito user pool.
--
-- * 'cipServerSideTokenCheck' - TRUE if server-side token validation is enabled for the identity provider
