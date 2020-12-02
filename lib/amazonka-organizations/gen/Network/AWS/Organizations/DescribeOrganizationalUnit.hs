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
-- Module      : Network.AWS.Organizations.DescribeOrganizationalUnit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an organizational unit (OU).
--
--
-- This operation can be called only from the organization's master account.
--
module Network.AWS.Organizations.DescribeOrganizationalUnit
    (
    -- * Creating a Request
      describeOrganizationalUnit
    , DescribeOrganizationalUnit
    -- * Request Lenses
    , douOrganizationalUnitId

    -- * Destructuring the Response
    , describeOrganizationalUnitResponse
    , DescribeOrganizationalUnitResponse
    -- * Response Lenses
    , doursOrganizationalUnit
    , doursResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeOrganizationalUnit' smart constructor.
newtype DescribeOrganizationalUnit = DescribeOrganizationalUnit'
  { _douOrganizationalUnitId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeOrganizationalUnit' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'douOrganizationalUnitId' - The unique identifier (ID) of the organizational unit that you want details about. You can get the ID from the 'ListOrganizationalUnitsForParent' operation. The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
describeOrganizationalUnit
    :: Text -- ^ 'douOrganizationalUnitId'
    -> DescribeOrganizationalUnit
describeOrganizationalUnit pOrganizationalUnitId_ =
  DescribeOrganizationalUnit'
    {_douOrganizationalUnitId = pOrganizationalUnitId_}


-- | The unique identifier (ID) of the organizational unit that you want details about. You can get the ID from the 'ListOrganizationalUnitsForParent' operation. The <http://wikipedia.org/wiki/regex regex pattern> for an organizational unit ID string requires "ou-" followed by from 4 to 32 lower-case letters or digits (the ID of the root that contains the OU) followed by a second "-" dash and from 8 to 32 additional lower-case letters or digits.
douOrganizationalUnitId :: Lens' DescribeOrganizationalUnit Text
douOrganizationalUnitId = lens _douOrganizationalUnitId (\ s a -> s{_douOrganizationalUnitId = a})

instance AWSRequest DescribeOrganizationalUnit where
        type Rs DescribeOrganizationalUnit =
             DescribeOrganizationalUnitResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 DescribeOrganizationalUnitResponse' <$>
                   (x .?> "OrganizationalUnit") <*> (pure (fromEnum s)))

instance Hashable DescribeOrganizationalUnit where

instance NFData DescribeOrganizationalUnit where

instance ToHeaders DescribeOrganizationalUnit where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DescribeOrganizationalUnit"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeOrganizationalUnit where
        toJSON DescribeOrganizationalUnit'{..}
          = object
              (catMaybes
                 [Just
                    ("OrganizationalUnitId" .=
                       _douOrganizationalUnitId)])

instance ToPath DescribeOrganizationalUnit where
        toPath = const "/"

instance ToQuery DescribeOrganizationalUnit where
        toQuery = const mempty

-- | /See:/ 'describeOrganizationalUnitResponse' smart constructor.
data DescribeOrganizationalUnitResponse = DescribeOrganizationalUnitResponse'
  { _doursOrganizationalUnit :: !(Maybe OrganizationalUnit)
  , _doursResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeOrganizationalUnitResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'doursOrganizationalUnit' - A structure that contains details about the specified OU.
--
-- * 'doursResponseStatus' - -- | The response status code.
describeOrganizationalUnitResponse
    :: Int -- ^ 'doursResponseStatus'
    -> DescribeOrganizationalUnitResponse
describeOrganizationalUnitResponse pResponseStatus_ =
  DescribeOrganizationalUnitResponse'
    { _doursOrganizationalUnit = Nothing
    , _doursResponseStatus = pResponseStatus_
    }


-- | A structure that contains details about the specified OU.
doursOrganizationalUnit :: Lens' DescribeOrganizationalUnitResponse (Maybe OrganizationalUnit)
doursOrganizationalUnit = lens _doursOrganizationalUnit (\ s a -> s{_doursOrganizationalUnit = a})

-- | -- | The response status code.
doursResponseStatus :: Lens' DescribeOrganizationalUnitResponse Int
doursResponseStatus = lens _doursResponseStatus (\ s a -> s{_doursResponseStatus = a})

instance NFData DescribeOrganizationalUnitResponse
         where
