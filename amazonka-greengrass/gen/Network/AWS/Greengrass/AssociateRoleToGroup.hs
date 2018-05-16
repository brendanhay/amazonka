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
-- Module      : Network.AWS.Greengrass.AssociateRoleToGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a role with a group. Your AWS Greengrass core will use the role to access AWS cloud services. The role's permissions should allow Greengrass core Lambda functions to perform actions against the cloud.
module Network.AWS.Greengrass.AssociateRoleToGroup
    (
    -- * Creating a Request
      associateRoleToGroup
    , AssociateRoleToGroup
    -- * Request Lenses
    , artgRoleARN
    , artgGroupId

    -- * Destructuring the Response
    , associateRoleToGroupResponse
    , AssociateRoleToGroupResponse
    -- * Response Lenses
    , artgrsAssociatedAt
    , artgrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateRoleToGroup' smart constructor.
data AssociateRoleToGroup = AssociateRoleToGroup'
  { _artgRoleARN :: !(Maybe Text)
  , _artgGroupId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateRoleToGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artgRoleARN' - The ARN of the role you wish to associate with this group.
--
-- * 'artgGroupId' - The ID of the AWS Greengrass group.
associateRoleToGroup
    :: Text -- ^ 'artgGroupId'
    -> AssociateRoleToGroup
associateRoleToGroup pGroupId_ =
  AssociateRoleToGroup' {_artgRoleARN = Nothing, _artgGroupId = pGroupId_}


-- | The ARN of the role you wish to associate with this group.
artgRoleARN :: Lens' AssociateRoleToGroup (Maybe Text)
artgRoleARN = lens _artgRoleARN (\ s a -> s{_artgRoleARN = a})

-- | The ID of the AWS Greengrass group.
artgGroupId :: Lens' AssociateRoleToGroup Text
artgGroupId = lens _artgGroupId (\ s a -> s{_artgGroupId = a})

instance AWSRequest AssociateRoleToGroup where
        type Rs AssociateRoleToGroup =
             AssociateRoleToGroupResponse
        request = putJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 AssociateRoleToGroupResponse' <$>
                   (x .?> "AssociatedAt") <*> (pure (fromEnum s)))

instance Hashable AssociateRoleToGroup where

instance NFData AssociateRoleToGroup where

instance ToHeaders AssociateRoleToGroup where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateRoleToGroup where
        toJSON AssociateRoleToGroup'{..}
          = object
              (catMaybes [("RoleArn" .=) <$> _artgRoleARN])

instance ToPath AssociateRoleToGroup where
        toPath AssociateRoleToGroup'{..}
          = mconcat
              ["/greengrass/groups/", toBS _artgGroupId, "/role"]

instance ToQuery AssociateRoleToGroup where
        toQuery = const mempty

-- | /See:/ 'associateRoleToGroupResponse' smart constructor.
data AssociateRoleToGroupResponse = AssociateRoleToGroupResponse'
  { _artgrsAssociatedAt   :: !(Maybe Text)
  , _artgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateRoleToGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artgrsAssociatedAt' - The time, in milliseconds since the epoch, when the role ARN was associated with the group.
--
-- * 'artgrsResponseStatus' - -- | The response status code.
associateRoleToGroupResponse
    :: Int -- ^ 'artgrsResponseStatus'
    -> AssociateRoleToGroupResponse
associateRoleToGroupResponse pResponseStatus_ =
  AssociateRoleToGroupResponse'
    {_artgrsAssociatedAt = Nothing, _artgrsResponseStatus = pResponseStatus_}


-- | The time, in milliseconds since the epoch, when the role ARN was associated with the group.
artgrsAssociatedAt :: Lens' AssociateRoleToGroupResponse (Maybe Text)
artgrsAssociatedAt = lens _artgrsAssociatedAt (\ s a -> s{_artgrsAssociatedAt = a})

-- | -- | The response status code.
artgrsResponseStatus :: Lens' AssociateRoleToGroupResponse Int
artgrsResponseStatus = lens _artgrsResponseStatus (\ s a -> s{_artgrsResponseStatus = a})

instance NFData AssociateRoleToGroupResponse where
