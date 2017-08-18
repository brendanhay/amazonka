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
-- Module      : Network.AWS.IAM.CreateRole
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new role for your AWS account. For more information about roles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> . For information about limitations on role names and the number of roles you can create, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
--
--
module Network.AWS.IAM.CreateRole
    (
    -- * Creating a Request
      createRole
    , CreateRole
    -- * Request Lenses
    , crPath
    , crDescription
    , crRoleName
    , crAssumeRolePolicyDocument

    -- * Destructuring the Response
    , createRoleResponse
    , CreateRoleResponse
    -- * Response Lenses
    , crrsResponseStatus
    , crrsRole
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createRole' smart constructor.
data CreateRole = CreateRole'
    { _crPath                     :: !(Maybe Text)
    , _crDescription              :: !(Maybe Text)
    , _crRoleName                 :: !Text
    , _crAssumeRolePolicyDocument :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crPath' - The path to the role. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This paramater allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes, containing any ASCII character from the ! (\u0021) thru the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'crDescription' - A customer-provided description of the role.
--
-- * 'crRoleName' - The name of the role to create. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@- Role names are not distinguished by case. For example, you cannot create roles named both "PRODROLE" and "prodrole".
--
-- * 'crAssumeRolePolicyDocument' - The trust relationship policy document that grants an entity permission to assume the role. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of any printable ASCII character ranging from the space character (\u0020) through end of the ASCII character range as well as the printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF). It also includes the special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D).
createRole
    :: Text -- ^ 'crRoleName'
    -> Text -- ^ 'crAssumeRolePolicyDocument'
    -> CreateRole
createRole pRoleName_ pAssumeRolePolicyDocument_ =
    CreateRole'
    { _crPath = Nothing
    , _crDescription = Nothing
    , _crRoleName = pRoleName_
    , _crAssumeRolePolicyDocument = pAssumeRolePolicyDocument_
    }

-- | The path to the role. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This paramater allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes, containing any ASCII character from the ! (\u0021) thru the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
crPath :: Lens' CreateRole (Maybe Text)
crPath = lens _crPath (\ s a -> s{_crPath = a});

-- | A customer-provided description of the role.
crDescription :: Lens' CreateRole (Maybe Text)
crDescription = lens _crDescription (\ s a -> s{_crDescription = a});

-- | The name of the role to create. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@- Role names are not distinguished by case. For example, you cannot create roles named both "PRODROLE" and "prodrole".
crRoleName :: Lens' CreateRole Text
crRoleName = lens _crRoleName (\ s a -> s{_crRoleName = a});

-- | The trust relationship policy document that grants an entity permission to assume the role. The <http://wikipedia.org/wiki/regex regex pattern> used to validate this parameter is a string of characters consisting of any printable ASCII character ranging from the space character (\u0020) through end of the ASCII character range as well as the printable characters in the Basic Latin and Latin-1 Supplement character set (through \u00FF). It also includes the special characters tab (\u0009), line feed (\u000A), and carriage return (\u000D).
crAssumeRolePolicyDocument :: Lens' CreateRole Text
crAssumeRolePolicyDocument = lens _crAssumeRolePolicyDocument (\ s a -> s{_crAssumeRolePolicyDocument = a});

instance AWSRequest CreateRole where
        type Rs CreateRole = CreateRoleResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "CreateRoleResult"
              (\ s h x ->
                 CreateRoleResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Role"))

instance Hashable CreateRole

instance NFData CreateRole

instance ToHeaders CreateRole where
        toHeaders = const mempty

instance ToPath CreateRole where
        toPath = const "/"

instance ToQuery CreateRole where
        toQuery CreateRole'{..}
          = mconcat
              ["Action" =: ("CreateRole" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _crPath, "Description" =: _crDescription,
               "RoleName" =: _crRoleName,
               "AssumeRolePolicyDocument" =:
                 _crAssumeRolePolicyDocument]

-- | Contains the response to a successful 'CreateRole' request.
--
--
--
-- /See:/ 'createRoleResponse' smart constructor.
data CreateRoleResponse = CreateRoleResponse'
    { _crrsResponseStatus :: !Int
    , _crrsRole           :: !Role
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsResponseStatus' - -- | The response status code.
--
-- * 'crrsRole' - A structure containing details about the new role.
createRoleResponse
    :: Int -- ^ 'crrsResponseStatus'
    -> Role -- ^ 'crrsRole'
    -> CreateRoleResponse
createRoleResponse pResponseStatus_ pRole_ =
    CreateRoleResponse'
    { _crrsResponseStatus = pResponseStatus_
    , _crrsRole = pRole_
    }

-- | -- | The response status code.
crrsResponseStatus :: Lens' CreateRoleResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\ s a -> s{_crrsResponseStatus = a});

-- | A structure containing details about the new role.
crrsRole :: Lens' CreateRoleResponse Role
crrsRole = lens _crrsRole (\ s a -> s{_crrsRole = a});

instance NFData CreateRoleResponse
