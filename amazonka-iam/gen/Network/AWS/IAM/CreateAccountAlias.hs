{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateAccountAlias
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for your AWS account. For information about using an
-- AWS account alias, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AccountAlias.html Using an Alias for Your AWS Account ID>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateAccountAlias.html>
module Network.AWS.IAM.CreateAccountAlias
    (
    -- * Request
      CreateAccountAlias
    -- ** Request constructor
    , createAccountAlias
    -- ** Request lenses
    , caarqAccountAlias

    -- * Response
    , CreateAccountAliasResponse
    -- ** Response constructor
    , createAccountAliasResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createAccountAlias' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'caarqAccountAlias'
newtype CreateAccountAlias = CreateAccountAlias'
    { _caarqAccountAlias :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAccountAlias' smart constructor.
createAccountAlias :: Text -> CreateAccountAlias
createAccountAlias pAccountAlias =
    CreateAccountAlias'
    { _caarqAccountAlias = pAccountAlias
    }

-- | The account alias to create.
caarqAccountAlias :: Lens' CreateAccountAlias Text
caarqAccountAlias = lens _caarqAccountAlias (\ s a -> s{_caarqAccountAlias = a});

instance AWSRequest CreateAccountAlias where
        type Sv CreateAccountAlias = IAM
        type Rs CreateAccountAlias =
             CreateAccountAliasResponse
        request = post
        response = receiveNull CreateAccountAliasResponse'

instance ToHeaders CreateAccountAlias where
        toHeaders = const mempty

instance ToPath CreateAccountAlias where
        toPath = const "/"

instance ToQuery CreateAccountAlias where
        toQuery CreateAccountAlias'{..}
          = mconcat
              ["Action" =: ("CreateAccountAlias" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "AccountAlias" =: _caarqAccountAlias]

-- | /See:/ 'createAccountAliasResponse' smart constructor.
data CreateAccountAliasResponse =
    CreateAccountAliasResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAccountAliasResponse' smart constructor.
createAccountAliasResponse :: CreateAccountAliasResponse
createAccountAliasResponse = CreateAccountAliasResponse'
