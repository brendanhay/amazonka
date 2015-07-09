{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateAccountAlias
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Creates an alias for your AWS account. For information about using an
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
    , caaAccountAlias

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
-- * 'caaAccountAlias'
newtype CreateAccountAlias = CreateAccountAlias'
    { _caaAccountAlias :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAccountAlias' smart constructor.
createAccountAlias :: Text -> CreateAccountAlias
createAccountAlias pAccountAlias =
    CreateAccountAlias'
    { _caaAccountAlias = pAccountAlias
    }

-- | The account alias to create.
caaAccountAlias :: Lens' CreateAccountAlias Text
caaAccountAlias = lens _caaAccountAlias (\ s a -> s{_caaAccountAlias = a});

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
               "AccountAlias" =: _caaAccountAlias]

-- | /See:/ 'createAccountAliasResponse' smart constructor.
data CreateAccountAliasResponse =
    CreateAccountAliasResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAccountAliasResponse' smart constructor.
createAccountAliasResponse :: CreateAccountAliasResponse
createAccountAliasResponse = CreateAccountAliasResponse'
