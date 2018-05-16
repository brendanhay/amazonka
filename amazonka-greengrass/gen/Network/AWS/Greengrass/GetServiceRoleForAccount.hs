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
-- Module      : Network.AWS.Greengrass.GetServiceRoleForAccount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the service role that is attached to your account.
module Network.AWS.Greengrass.GetServiceRoleForAccount
    (
    -- * Creating a Request
      getServiceRoleForAccount
    , GetServiceRoleForAccount

    -- * Destructuring the Response
    , getServiceRoleForAccountResponse
    , GetServiceRoleForAccountResponse
    -- * Response Lenses
    , gsrfarsAssociatedAt
    , gsrfarsRoleARN
    , gsrfarsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getServiceRoleForAccount' smart constructor.
data GetServiceRoleForAccount =
  GetServiceRoleForAccount'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServiceRoleForAccount' with the minimum fields required to make a request.
--
getServiceRoleForAccount
    :: GetServiceRoleForAccount
getServiceRoleForAccount = GetServiceRoleForAccount'


instance AWSRequest GetServiceRoleForAccount where
        type Rs GetServiceRoleForAccount =
             GetServiceRoleForAccountResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetServiceRoleForAccountResponse' <$>
                   (x .?> "AssociatedAt") <*> (x .?> "RoleArn") <*>
                     (pure (fromEnum s)))

instance Hashable GetServiceRoleForAccount where

instance NFData GetServiceRoleForAccount where

instance ToHeaders GetServiceRoleForAccount where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetServiceRoleForAccount where
        toPath = const "/greengrass/servicerole"

instance ToQuery GetServiceRoleForAccount where
        toQuery = const mempty

-- | /See:/ 'getServiceRoleForAccountResponse' smart constructor.
data GetServiceRoleForAccountResponse = GetServiceRoleForAccountResponse'
  { _gsrfarsAssociatedAt   :: !(Maybe Text)
  , _gsrfarsRoleARN        :: !(Maybe Text)
  , _gsrfarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetServiceRoleForAccountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gsrfarsAssociatedAt' - The time when the service role was associated with the account.
--
-- * 'gsrfarsRoleARN' - The ARN of the role which is associated with the account.
--
-- * 'gsrfarsResponseStatus' - -- | The response status code.
getServiceRoleForAccountResponse
    :: Int -- ^ 'gsrfarsResponseStatus'
    -> GetServiceRoleForAccountResponse
getServiceRoleForAccountResponse pResponseStatus_ =
  GetServiceRoleForAccountResponse'
    { _gsrfarsAssociatedAt = Nothing
    , _gsrfarsRoleARN = Nothing
    , _gsrfarsResponseStatus = pResponseStatus_
    }


-- | The time when the service role was associated with the account.
gsrfarsAssociatedAt :: Lens' GetServiceRoleForAccountResponse (Maybe Text)
gsrfarsAssociatedAt = lens _gsrfarsAssociatedAt (\ s a -> s{_gsrfarsAssociatedAt = a})

-- | The ARN of the role which is associated with the account.
gsrfarsRoleARN :: Lens' GetServiceRoleForAccountResponse (Maybe Text)
gsrfarsRoleARN = lens _gsrfarsRoleARN (\ s a -> s{_gsrfarsRoleARN = a})

-- | -- | The response status code.
gsrfarsResponseStatus :: Lens' GetServiceRoleForAccountResponse Int
gsrfarsResponseStatus = lens _gsrfarsResponseStatus (\ s a -> s{_gsrfarsResponseStatus = a})

instance NFData GetServiceRoleForAccountResponse
         where
