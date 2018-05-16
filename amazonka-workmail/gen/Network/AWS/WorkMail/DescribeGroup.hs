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
-- Module      : Network.AWS.WorkMail.DescribeGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data available for the group.
--
--
module Network.AWS.WorkMail.DescribeGroup
    (
    -- * Creating a Request
      describeGroup
    , DescribeGroup
    -- * Request Lenses
    , dgOrganizationId
    , dgGroupId

    -- * Destructuring the Response
    , describeGroupResponse
    , DescribeGroupResponse
    -- * Response Lenses
    , desrsEmail
    , desrsState
    , desrsDisabledDate
    , desrsName
    , desrsGroupId
    , desrsEnabledDate
    , desrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkMail.Types
import Network.AWS.WorkMail.Types.Product

-- | /See:/ 'describeGroup' smart constructor.
data DescribeGroup = DescribeGroup'
  { _dgOrganizationId :: !Text
  , _dgGroupId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgOrganizationId' - The identifier for the organization under which the group exists.
--
-- * 'dgGroupId' - The identifier for the group to be described.
describeGroup
    :: Text -- ^ 'dgOrganizationId'
    -> Text -- ^ 'dgGroupId'
    -> DescribeGroup
describeGroup pOrganizationId_ pGroupId_ =
  DescribeGroup' {_dgOrganizationId = pOrganizationId_, _dgGroupId = pGroupId_}


-- | The identifier for the organization under which the group exists.
dgOrganizationId :: Lens' DescribeGroup Text
dgOrganizationId = lens _dgOrganizationId (\ s a -> s{_dgOrganizationId = a})

-- | The identifier for the group to be described.
dgGroupId :: Lens' DescribeGroup Text
dgGroupId = lens _dgGroupId (\ s a -> s{_dgGroupId = a})

instance AWSRequest DescribeGroup where
        type Rs DescribeGroup = DescribeGroupResponse
        request = postJSON workMail
        response
          = receiveJSON
              (\ s h x ->
                 DescribeGroupResponse' <$>
                   (x .?> "Email") <*> (x .?> "State") <*>
                     (x .?> "DisabledDate")
                     <*> (x .?> "Name")
                     <*> (x .?> "GroupId")
                     <*> (x .?> "EnabledDate")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeGroup where

instance NFData DescribeGroup where

instance ToHeaders DescribeGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkMailService.DescribeGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeGroup where
        toJSON DescribeGroup'{..}
          = object
              (catMaybes
                 [Just ("OrganizationId" .= _dgOrganizationId),
                  Just ("GroupId" .= _dgGroupId)])

instance ToPath DescribeGroup where
        toPath = const "/"

instance ToQuery DescribeGroup where
        toQuery = const mempty

-- | /See:/ 'describeGroupResponse' smart constructor.
data DescribeGroupResponse = DescribeGroupResponse'
  { _desrsEmail          :: !(Maybe Text)
  , _desrsState          :: !(Maybe EntityState)
  , _desrsDisabledDate   :: !(Maybe POSIX)
  , _desrsName           :: !(Maybe Text)
  , _desrsGroupId        :: !(Maybe Text)
  , _desrsEnabledDate    :: !(Maybe POSIX)
  , _desrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsEmail' - The email of the described group.
--
-- * 'desrsState' - The state of the user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to Amazon WorkMail).
--
-- * 'desrsDisabledDate' - The date and time when a user was deregistered from Amazon WorkMail, in UNIX epoch time format.
--
-- * 'desrsName' - The name of the described group.
--
-- * 'desrsGroupId' - The identifier of the described group.
--
-- * 'desrsEnabledDate' - The date and time when a user was registered to Amazon WorkMail, in UNIX epoch time format.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeGroupResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeGroupResponse
describeGroupResponse pResponseStatus_ =
  DescribeGroupResponse'
    { _desrsEmail = Nothing
    , _desrsState = Nothing
    , _desrsDisabledDate = Nothing
    , _desrsName = Nothing
    , _desrsGroupId = Nothing
    , _desrsEnabledDate = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }


-- | The email of the described group.
desrsEmail :: Lens' DescribeGroupResponse (Maybe Text)
desrsEmail = lens _desrsEmail (\ s a -> s{_desrsEmail = a})

-- | The state of the user: enabled (registered to Amazon WorkMail) or disabled (deregistered or never registered to Amazon WorkMail).
desrsState :: Lens' DescribeGroupResponse (Maybe EntityState)
desrsState = lens _desrsState (\ s a -> s{_desrsState = a})

-- | The date and time when a user was deregistered from Amazon WorkMail, in UNIX epoch time format.
desrsDisabledDate :: Lens' DescribeGroupResponse (Maybe UTCTime)
desrsDisabledDate = lens _desrsDisabledDate (\ s a -> s{_desrsDisabledDate = a}) . mapping _Time

-- | The name of the described group.
desrsName :: Lens' DescribeGroupResponse (Maybe Text)
desrsName = lens _desrsName (\ s a -> s{_desrsName = a})

-- | The identifier of the described group.
desrsGroupId :: Lens' DescribeGroupResponse (Maybe Text)
desrsGroupId = lens _desrsGroupId (\ s a -> s{_desrsGroupId = a})

-- | The date and time when a user was registered to Amazon WorkMail, in UNIX epoch time format.
desrsEnabledDate :: Lens' DescribeGroupResponse (Maybe UTCTime)
desrsEnabledDate = lens _desrsEnabledDate (\ s a -> s{_desrsEnabledDate = a}) . mapping _Time

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeGroupResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeGroupResponse where
