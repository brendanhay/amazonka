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
-- Module      : Network.AWS.Organizations.DescribeOrganization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the organization that the user's account belongs to.
--
--
-- This operation can be called from any account in the organization.
--
module Network.AWS.Organizations.DescribeOrganization
    (
    -- * Creating a Request
      describeOrganization
    , DescribeOrganization

    -- * Destructuring the Response
    , describeOrganizationResponse
    , DescribeOrganizationResponse
    -- * Response Lenses
    , dorsOrganization
    , dorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Organizations.Types
import Network.AWS.Organizations.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeOrganization' smart constructor.
data DescribeOrganization =
  DescribeOrganization'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeOrganization' with the minimum fields required to make a request.
--
describeOrganization
    :: DescribeOrganization
describeOrganization = DescribeOrganization'


instance AWSRequest DescribeOrganization where
        type Rs DescribeOrganization =
             DescribeOrganizationResponse
        request = postJSON organizations
        response
          = receiveJSON
              (\ s h x ->
                 DescribeOrganizationResponse' <$>
                   (x .?> "Organization") <*> (pure (fromEnum s)))

instance Hashable DescribeOrganization where

instance NFData DescribeOrganization where

instance ToHeaders DescribeOrganization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSOrganizationsV20161128.DescribeOrganization" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeOrganization where
        toJSON = const (Object mempty)

instance ToPath DescribeOrganization where
        toPath = const "/"

instance ToQuery DescribeOrganization where
        toQuery = const mempty

-- | /See:/ 'describeOrganizationResponse' smart constructor.
data DescribeOrganizationResponse = DescribeOrganizationResponse'
  { _dorsOrganization   :: !(Maybe Organization)
  , _dorsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeOrganizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dorsOrganization' - A structure that contains information about the organization.
--
-- * 'dorsResponseStatus' - -- | The response status code.
describeOrganizationResponse
    :: Int -- ^ 'dorsResponseStatus'
    -> DescribeOrganizationResponse
describeOrganizationResponse pResponseStatus_ =
  DescribeOrganizationResponse'
    {_dorsOrganization = Nothing, _dorsResponseStatus = pResponseStatus_}


-- | A structure that contains information about the organization.
dorsOrganization :: Lens' DescribeOrganizationResponse (Maybe Organization)
dorsOrganization = lens _dorsOrganization (\ s a -> s{_dorsOrganization = a})

-- | -- | The response status code.
dorsResponseStatus :: Lens' DescribeOrganizationResponse Int
dorsResponseStatus = lens _dorsResponseStatus (\ s a -> s{_dorsResponseStatus = a})

instance NFData DescribeOrganizationResponse where
