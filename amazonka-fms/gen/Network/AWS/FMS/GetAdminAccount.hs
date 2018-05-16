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
-- Module      : Network.AWS.FMS.GetAdminAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the AWS Organizations master account that is associated with AWS Firewall Manager as the AWS Firewall Manager administrator.
--
--
module Network.AWS.FMS.GetAdminAccount
    (
    -- * Creating a Request
      getAdminAccount
    , GetAdminAccount

    -- * Destructuring the Response
    , getAdminAccountResponse
    , GetAdminAccountResponse
    -- * Response Lenses
    , gaarsAdminAccount
    , gaarsResponseStatus
    ) where

import Network.AWS.FMS.Types
import Network.AWS.FMS.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAdminAccount' smart constructor.
data GetAdminAccount =
  GetAdminAccount'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAdminAccount' with the minimum fields required to make a request.
--
getAdminAccount
    :: GetAdminAccount
getAdminAccount = GetAdminAccount'


instance AWSRequest GetAdminAccount where
        type Rs GetAdminAccount = GetAdminAccountResponse
        request = postJSON fms
        response
          = receiveJSON
              (\ s h x ->
                 GetAdminAccountResponse' <$>
                   (x .?> "AdminAccount") <*> (pure (fromEnum s)))

instance Hashable GetAdminAccount where

instance NFData GetAdminAccount where

instance ToHeaders GetAdminAccount where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSFMS_20180101.GetAdminAccount" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetAdminAccount where
        toJSON = const (Object mempty)

instance ToPath GetAdminAccount where
        toPath = const "/"

instance ToQuery GetAdminAccount where
        toQuery = const mempty

-- | /See:/ 'getAdminAccountResponse' smart constructor.
data GetAdminAccountResponse = GetAdminAccountResponse'
  { _gaarsAdminAccount   :: !(Maybe Text)
  , _gaarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAdminAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaarsAdminAccount' - The AWS account that is set as the AWS Firewall Manager administrator.
--
-- * 'gaarsResponseStatus' - -- | The response status code.
getAdminAccountResponse
    :: Int -- ^ 'gaarsResponseStatus'
    -> GetAdminAccountResponse
getAdminAccountResponse pResponseStatus_ =
  GetAdminAccountResponse'
    {_gaarsAdminAccount = Nothing, _gaarsResponseStatus = pResponseStatus_}


-- | The AWS account that is set as the AWS Firewall Manager administrator.
gaarsAdminAccount :: Lens' GetAdminAccountResponse (Maybe Text)
gaarsAdminAccount = lens _gaarsAdminAccount (\ s a -> s{_gaarsAdminAccount = a})

-- | -- | The response status code.
gaarsResponseStatus :: Lens' GetAdminAccountResponse Int
gaarsResponseStatus = lens _gaarsResponseStatus (\ s a -> s{_gaarsResponseStatus = a})

instance NFData GetAdminAccountResponse where
