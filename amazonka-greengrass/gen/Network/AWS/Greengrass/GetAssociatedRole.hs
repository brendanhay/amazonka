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
-- Module      : Network.AWS.Greengrass.GetAssociatedRole
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the role associated with a particular group.
module Network.AWS.Greengrass.GetAssociatedRole
    (
    -- * Creating a Request
      getAssociatedRole
    , GetAssociatedRole
    -- * Request Lenses
    , garGroupId

    -- * Destructuring the Response
    , getAssociatedRoleResponse
    , GetAssociatedRoleResponse
    -- * Response Lenses
    , garrsAssociatedAt
    , garrsRoleARN
    , garrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAssociatedRole' smart constructor.
newtype GetAssociatedRole = GetAssociatedRole'
  { _garGroupId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAssociatedRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garGroupId' - The ID of the AWS Greengrass group.
getAssociatedRole
    :: Text -- ^ 'garGroupId'
    -> GetAssociatedRole
getAssociatedRole pGroupId_ = GetAssociatedRole' {_garGroupId = pGroupId_}


-- | The ID of the AWS Greengrass group.
garGroupId :: Lens' GetAssociatedRole Text
garGroupId = lens _garGroupId (\ s a -> s{_garGroupId = a})

instance AWSRequest GetAssociatedRole where
        type Rs GetAssociatedRole = GetAssociatedRoleResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetAssociatedRoleResponse' <$>
                   (x .?> "AssociatedAt") <*> (x .?> "RoleArn") <*>
                     (pure (fromEnum s)))

instance Hashable GetAssociatedRole where

instance NFData GetAssociatedRole where

instance ToHeaders GetAssociatedRole where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetAssociatedRole where
        toPath GetAssociatedRole'{..}
          = mconcat
              ["/greengrass/groups/", toBS _garGroupId, "/role"]

instance ToQuery GetAssociatedRole where
        toQuery = const mempty

-- | /See:/ 'getAssociatedRoleResponse' smart constructor.
data GetAssociatedRoleResponse = GetAssociatedRoleResponse'
  { _garrsAssociatedAt   :: !(Maybe Text)
  , _garrsRoleARN        :: !(Maybe Text)
  , _garrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAssociatedRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'garrsAssociatedAt' - The time when the role was associated with the group.
--
-- * 'garrsRoleARN' - The ARN of the role that is associated with the group.
--
-- * 'garrsResponseStatus' - -- | The response status code.
getAssociatedRoleResponse
    :: Int -- ^ 'garrsResponseStatus'
    -> GetAssociatedRoleResponse
getAssociatedRoleResponse pResponseStatus_ =
  GetAssociatedRoleResponse'
    { _garrsAssociatedAt = Nothing
    , _garrsRoleARN = Nothing
    , _garrsResponseStatus = pResponseStatus_
    }


-- | The time when the role was associated with the group.
garrsAssociatedAt :: Lens' GetAssociatedRoleResponse (Maybe Text)
garrsAssociatedAt = lens _garrsAssociatedAt (\ s a -> s{_garrsAssociatedAt = a})

-- | The ARN of the role that is associated with the group.
garrsRoleARN :: Lens' GetAssociatedRoleResponse (Maybe Text)
garrsRoleARN = lens _garrsRoleARN (\ s a -> s{_garrsRoleARN = a})

-- | -- | The response status code.
garrsResponseStatus :: Lens' GetAssociatedRoleResponse Int
garrsResponseStatus = lens _garrsResponseStatus (\ s a -> s{_garrsResponseStatus = a})

instance NFData GetAssociatedRoleResponse where
