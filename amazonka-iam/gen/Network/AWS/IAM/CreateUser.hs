{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateUser
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user for your AWS account.
--
-- For information about limitations on the number of users you can create,
-- see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateUser.html AWS API Reference> for CreateUser.
module Network.AWS.IAM.CreateUser
    (
    -- * Creating a Request
      CreateUser
    , createUser
    -- * Request Lenses
    , cuPath
    , cuUserName

    -- * Destructuring the Response
    , CreateUserResponse
    , createUserResponse
    -- * Response Lenses
    , cursUser
    , cursStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUser' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cuPath'
--
-- * 'cuUserName'
data CreateUser = CreateUser'
    { _cuPath :: !(Maybe Text)
    , _cuUserName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateUser' smart constructor.
createUser :: Text -> CreateUser
createUser pUserName_ = 
    CreateUser'
    { _cuPath = Nothing
    , _cuUserName = pUserName_
    }

-- | The path for the user name. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
cuPath :: Lens' CreateUser (Maybe Text)
cuPath = lens _cuPath (\ s a -> s{_cuPath = a});

-- | The name of the user to create.
cuUserName :: Lens' CreateUser Text
cuUserName = lens _cuUserName (\ s a -> s{_cuUserName = a});

instance AWSRequest CreateUser where
        type Sv CreateUser = IAM
        type Rs CreateUser = CreateUserResponse
        request = postQuery
        response
          = receiveXMLWrapper "CreateUserResult"
              (\ s h x ->
                 CreateUserResponse' <$>
                   (x .@? "User") <*> (pure (fromEnum s)))

instance ToHeaders CreateUser where
        toHeaders = const mempty

instance ToPath CreateUser where
        toPath = const "/"

instance ToQuery CreateUser where
        toQuery CreateUser'{..}
          = mconcat
              ["Action" =: ("CreateUser" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _cuPath, "UserName" =: _cuUserName]

-- | Contains the response to a successful CreateUser request.
--
-- /See:/ 'createUserResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cursUser'
--
-- * 'cursStatus'
data CreateUserResponse = CreateUserResponse'
    { _cursUser :: !(Maybe User)
    , _cursStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateUserResponse' smart constructor.
createUserResponse :: Int -> CreateUserResponse
createUserResponse pStatus_ = 
    CreateUserResponse'
    { _cursUser = Nothing
    , _cursStatus = pStatus_
    }

-- | Information about the user.
cursUser :: Lens' CreateUserResponse (Maybe User)
cursUser = lens _cursUser (\ s a -> s{_cursUser = a});

-- | Undocumented member.
cursStatus :: Lens' CreateUserResponse Int
cursStatus = lens _cursStatus (\ s a -> s{_cursStatus = a});
