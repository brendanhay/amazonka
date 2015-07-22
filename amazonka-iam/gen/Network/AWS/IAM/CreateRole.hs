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
-- Stability   : experimental
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
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html>
module Network.AWS.IAM.CreateRole
    (
    -- * Request
      CreateRole
    -- ** Request constructor
    , createRole
    -- ** Request lenses
    , crrqPath
    , crrqRoleName
    , crrqAssumeRolePolicyDocument

    -- * Response
    , CreateRoleResponse
    -- ** Response constructor
    , createRoleResponse
    -- ** Response lenses
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
-- * 'crrqPath'
--
-- * 'crrqRoleName'
--
-- * 'crrqAssumeRolePolicyDocument'
data CreateRole = CreateRole'
    { _crrqPath                     :: !(Maybe Text)
    , _crrqRoleName                 :: !Text
    , _crrqAssumeRolePolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateRole' smart constructor.
createRole :: Text -> Text -> CreateRole
createRole pRoleName pAssumeRolePolicyDocument =
    CreateRole'
    { _crrqPath = Nothing
    , _crrqRoleName = pRoleName
    , _crrqAssumeRolePolicyDocument = pAssumeRolePolicyDocument
    }

-- | The path to the role. For more information about paths, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
crrqPath :: Lens' CreateRole (Maybe Text)
crrqPath = lens _crrqPath (\ s a -> s{_crrqPath = a});

-- | The name of the role to create.
crrqRoleName :: Lens' CreateRole Text
crrqRoleName = lens _crrqRoleName (\ s a -> s{_crrqRoleName = a});

-- | The policy that grants an entity permission to assume the role.
crrqAssumeRolePolicyDocument :: Lens' CreateRole Text
crrqAssumeRolePolicyDocument = lens _crrqAssumeRolePolicyDocument (\ s a -> s{_crrqAssumeRolePolicyDocument = a});

instance AWSRequest CreateRole where
        type Sv CreateRole = IAM
        type Rs CreateRole = CreateRoleResponse
        request = post
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
               "Path" =: _crrqPath, "RoleName" =: _crrqRoleName,
               "AssumeRolePolicyDocument" =:
                 _crrqAssumeRolePolicyDocument]

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
createRoleResponse pStatus pRole =
    CreateRoleResponse'
    { _crrsStatus = pStatus
    , _crrsRole = pRole
    }

-- | FIXME: Undocumented member.
crrsStatus :: Lens' CreateRoleResponse Int
crrsStatus = lens _crrsStatus (\ s a -> s{_crrsStatus = a});

-- | Information about the role.
crrsRole :: Lens' CreateRoleResponse Role
crrsRole = lens _crrsRole (\ s a -> s{_crrsRole = a});
