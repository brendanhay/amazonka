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
-- Module      : Network.AWS.Organizations.UpdateOrganizationalUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renames the specified organizational unit (OU). The ID and ARN do not change. The child OUs and accounts remain in place, and any attached policies of the OU remain attached.
--
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.UpdateOrganizationalUnit
    (
    -- * Creating a Request
      updateOrganizationalUnit
    , UpdateOrganizationalUnit
    -- * Request Lenses
    , uouName
    , uouOrganizationalUnitId

    -- * Destructuring the Response
    , updateOrganizationalUnitResponse
    , UpdateOrganizationalUnitResponse
    -- * Response Lenses
    , uoursOrganizationalUnit
    , uoursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateOrganizationalUnit' smart constructor.
data UpdateOrganizationalUnit = UpdateOrganizationalUnit'
  { _uouName                 :: !(Maybe Text)
  , _uouOrganizationalUnitId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateOrganizationalUnit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uouName' - The new name that you want to assign to the OU. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'uouOrganizationalUnitId' - The unique identifier (ID) of the OU that you want to rename. You can get the ID from the 'ListOrganizationalUnitsForParent' operation. The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
updateOrganizationalUnit
    :: Text -- ^ 'uouOrganizationalUnitId'
    -> UpdateOrganizationalUnit
updateOrganizationalUnit pOrganizationalUnitId_ =
  UpdateOrganizationalUnit'
    {_uouName = Nothing, _uouOrganizationalUnitId = pOrganizationalUnitId_}


-- | The new name that you want to assign to the OU. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
uouName :: Lens' UpdateOrganizationalUnit (Maybe Text)
uouName = lens _uouName (\ s a -> s{_uouName = a})

-- | The unique identifier (ID) of the OU that you want to rename. You can get the ID from the 'ListOrganizationalUnitsForParent' operation. The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
uouOrganizationalUnitId :: Lens' UpdateOrganizationalUnit Text
uouOrganizationalUnitId = lens _uouOrganizationalUnitId (\ s a -> s{_uouOrganizationalUnitId = a})

instance AWSRequest UpdateOrganizationalUnit where
        type Rs UpdateOrganizationalUnit =
             UpdateOrganizationalUnitResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 UpdateOrganizationalUnitResponse' <$>
                   (x .?> "OrganizationalUnit") <*> (pure (fromEnum s)))

instance Hashable UpdateOrganizationalUnit where

instance NFData UpdateOrganizationalUnit where

instance ToHeaders UpdateOrganizationalUnit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.UpdateOrganizationalUnit"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateOrganizationalUnit where
        toJSON UpdateOrganizationalUnit'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _uouName,
                  Just
                    ("OrganizationalUnitId" .=
                       _uouOrganizationalUnitId)])

instance ToPath UpdateOrganizationalUnit where
        toPath = const "/"

instance ToQuery UpdateOrganizationalUnit where
        toQuery = const mempty

-- | /See:/ 'updateOrganizationalUnitResponse' smart constructor.
data UpdateOrganizationalUnitResponse = UpdateOrganizationalUnitResponse'
  { _uoursOrganizationalUnit :: !(Maybe OrganizationalUnit)
  , _uoursResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateOrganizationalUnitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uoursOrganizationalUnit' - A structure that contains the details about the specified OU, including its new name.
--
-- * 'uoursResponseStatus' - -- | The response status code.
updateOrganizationalUnitResponse
    :: Int -- ^ 'uoursResponseStatus'
    -> UpdateOrganizationalUnitResponse
updateOrganizationalUnitResponse pResponseStatus_ =
  UpdateOrganizationalUnitResponse'
    { _uoursOrganizationalUnit = Nothing
    , _uoursResponseStatus = pResponseStatus_
    }


-- | A structure that contains the details about the specified OU, including its new name.
uoursOrganizationalUnit :: Lens' UpdateOrganizationalUnitResponse (Maybe OrganizationalUnit)
uoursOrganizationalUnit = lens _uoursOrganizationalUnit (\ s a -> s{_uoursOrganizationalUnit = a})

-- | -- | The response status code.
uoursResponseStatus :: Lens' UpdateOrganizationalUnitResponse Int
uoursResponseStatus = lens _uoursResponseStatus (\ s a -> s{_uoursResponseStatus = a})

instance NFData UpdateOrganizationalUnitResponse
         where
