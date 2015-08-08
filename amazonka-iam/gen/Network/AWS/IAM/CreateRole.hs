{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateRole
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new role for your AWS account. For more information about
-- roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>.
-- For information about limitations on role names and the number of roles
-- you can create, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
--
-- The policy in the following example grants permission to an EC2 instance
-- to assume the role.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html AWS API Reference> for CreateRole.
module Network.AWS.IAM.CreateRole
    (
    -- * Creating a Request
      CreateRole
    , createRole
    -- * Request Lenses
    , crPath
    , crRoleName
    , crAssumeRolePolicyDocument

    -- * Destructuring the Response
    , CreateRoleResponse
    , createRoleResponse
    -- * Response Lenses
    , crrsStatus
    , crrsRole
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crPath'
--
-- * 'crRoleName'
--
-- * 'crAssumeRolePolicyDocument'
data CreateRole = CreateRole'
    { _crPath                     :: !(Maybe Text)
    , _crRoleName                 :: !Text
    , _crAssumeRolePolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateRole' smart constructor.
createRole :: Text -> Text -> CreateRole
createRole pRoleName_ pAssumeRolePolicyDocument_ =
    CreateRole'
    { _crPath = Nothing
    , _crRoleName = pRoleName_
    , _crAssumeRolePolicyDocument = pAssumeRolePolicyDocument_
    }

-- | The path to the role. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
crPath :: Lens' CreateRole (Maybe Text)
crPath = lens _crPath (\ s a -> s{_crPath = a});

-- | The name of the role to create.
crRoleName :: Lens' CreateRole Text
crRoleName = lens _crRoleName (\ s a -> s{_crRoleName = a});

-- | The policy that grants an entity permission to assume the role.
crAssumeRolePolicyDocument :: Lens' CreateRole Text
crAssumeRolePolicyDocument = lens _crAssumeRolePolicyDocument (\ s a -> s{_crAssumeRolePolicyDocument = a});

instance AWSRequest CreateRole where
        type Sv CreateRole = IAM
        type Rs CreateRole = CreateRoleResponse
        request = postQuery
        response
          = receiveXMLWrapper "CreateRoleResult"
              (\ s h x ->
                 CreateRoleResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Role"))

instance ToHeaders CreateRole where
        toHeaders = const mempty

instance ToPath CreateRole where
        toPath = const "/"

instance ToQuery CreateRole where
        toQuery CreateRole'{..}
          = mconcat
              ["Action" =: ("CreateRole" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _crPath, "RoleName" =: _crRoleName,
               "AssumeRolePolicyDocument" =:
                 _crAssumeRolePolicyDocument]

-- | Contains the response to a successful CreateRole request.
--
-- /See:/ 'createRoleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crrsStatus'
--
-- * 'crrsRole'
data CreateRoleResponse = CreateRoleResponse'
    { _crrsStatus :: !Int
    , _crrsRole   :: !Role
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateRoleResponse' smart constructor.
createRoleResponse :: Int -> Role -> CreateRoleResponse
createRoleResponse pStatus_ pRole_ =
    CreateRoleResponse'
    { _crrsStatus = pStatus_
    , _crrsRole = pRole_
    }

-- | Undocumented member.
crrsStatus :: Lens' CreateRoleResponse Int
crrsStatus = lens _crrsStatus (\ s a -> s{_crrsStatus = a});

-- | Information about the role.
crrsRole :: Lens' CreateRoleResponse Role
crrsRole = lens _crrsRole (\ s a -> s{_crrsRole = a});
