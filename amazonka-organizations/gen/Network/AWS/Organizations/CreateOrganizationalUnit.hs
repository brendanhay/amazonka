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
-- Module      : Network.AWS.Organizations.CreateOrganizationalUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an organizational unit (OU) within a root or parent OU. An OU is a container for accounts that enables you to organize your accounts to apply policies according to your business requirements. The number of levels deep that you can nest OUs is dependent upon the policy types enabled for that root. For service control policies, the limit is five.
--
--
-- For more information about OUs, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_ous.html Managing Organizational Units> in the /AWS Organizations User Guide/ .
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.CreateOrganizationalUnit
    (
    -- * Creating a Request
      createOrganizationalUnit
    , CreateOrganizationalUnit
    -- * Request Lenses
    , couParentId
    , couName

    -- * Destructuring the Response
    , createOrganizationalUnitResponse
    , CreateOrganizationalUnitResponse
    -- * Response Lenses
    , coursOrganizationalUnit
    , coursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createOrganizationalUnit' smart constructor.
data CreateOrganizationalUnit = CreateOrganizationalUnit'
  { _couParentId :: !Text
  , _couName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateOrganizationalUnit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'couParentId' - The unique identifier (ID) of the parent root or OU in which you want to create the new OU. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
--
-- * 'couName' - The friendly name to assign to the new OU.
createOrganizationalUnit
    :: Text -- ^ 'couParentId'
    -> Text -- ^ 'couName'
    -> CreateOrganizationalUnit
createOrganizationalUnit pParentId_ pName_ =
  CreateOrganizationalUnit' {_couParentId = pParentId_, _couName = pName_}


-- | The unique identifier (ID) of the parent root or OU in which you want to create the new OU. The <http://wikipedia.org/wiki/regex regex pattern> for a parent ID string requires one of the following:     * Root: a string that begins with "r-" followed by from 4 to 32 lower-case letters or digits.     * Organizational unit (OU): a string that begins with "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that the OU is in) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
couParentId :: Lens' CreateOrganizationalUnit Text
couParentId = lens _couParentId (\ s a -> s{_couParentId = a})

-- | The friendly name to assign to the new OU.
couName :: Lens' CreateOrganizationalUnit Text
couName = lens _couName (\ s a -> s{_couName = a})

instance AWSRequest CreateOrganizationalUnit where
        type Rs CreateOrganizationalUnit =
             CreateOrganizationalUnitResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 CreateOrganizationalUnitResponse' <$>
                   (x .?> "OrganizationalUnit") <*> (pure (fromEnum s)))

instance Hashable CreateOrganizationalUnit where

instance NFData CreateOrganizationalUnit where

instance ToHeaders CreateOrganizationalUnit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.CreateOrganizationalUnit"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateOrganizationalUnit where
        toJSON CreateOrganizationalUnit'{..}
          = object
              (catMaybes
                 [Just ("ParentId" .= _couParentId),
                  Just ("Name" .= _couName)])

instance ToPath CreateOrganizationalUnit where
        toPath = const "/"

instance ToQuery CreateOrganizationalUnit where
        toQuery = const mempty

-- | /See:/ 'createOrganizationalUnitResponse' smart constructor.
data CreateOrganizationalUnitResponse = CreateOrganizationalUnitResponse'
  { _coursOrganizationalUnit :: !(Maybe OrganizationalUnit)
  , _coursResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateOrganizationalUnitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coursOrganizationalUnit' - A structure that contains details about the newly created OU.
--
-- * 'coursResponseStatus' - -- | The response status code.
createOrganizationalUnitResponse
    :: Int -- ^ 'coursResponseStatus'
    -> CreateOrganizationalUnitResponse
createOrganizationalUnitResponse pResponseStatus_ =
  CreateOrganizationalUnitResponse'
    { _coursOrganizationalUnit = Nothing
    , _coursResponseStatus = pResponseStatus_
    }


-- | A structure that contains details about the newly created OU.
coursOrganizationalUnit :: Lens' CreateOrganizationalUnitResponse (Maybe OrganizationalUnit)
coursOrganizationalUnit = lens _coursOrganizationalUnit (\ s a -> s{_coursOrganizationalUnit = a})

-- | -- | The response status code.
coursResponseStatus :: Lens' CreateOrganizationalUnitResponse Int
coursResponseStatus = lens _coursResponseStatus (\ s a -> s{_coursResponseStatus = a})

instance NFData CreateOrganizationalUnitResponse
         where
