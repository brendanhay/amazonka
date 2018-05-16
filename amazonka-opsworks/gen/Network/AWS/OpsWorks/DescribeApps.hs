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
-- Module      : Network.AWS.OpsWorks.DescribeApps
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a specified set of apps.
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Show, Deploy, or Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.DescribeApps
    (
    -- * Creating a Request
      describeApps
    , DescribeApps
    -- * Request Lenses
    , daAppIds
    , daStackId

    -- * Destructuring the Response
    , describeAppsResponse
    , DescribeAppsResponse
    -- * Response Lenses
    , darsApps
    , darsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeApps' smart constructor.
data DescribeApps = DescribeApps'
  { _daAppIds  :: !(Maybe [Text])
  , _daStackId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeApps' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAppIds' - An array of app IDs for the apps to be described. If you use this parameter, @DescribeApps@ returns a description of the specified apps. Otherwise, it returns a description of every app.
--
-- * 'daStackId' - The app stack ID. If you use this parameter, @DescribeApps@ returns a description of the apps in the specified stack.
describeApps
    :: DescribeApps
describeApps = DescribeApps' {_daAppIds = Nothing, _daStackId = Nothing}


-- | An array of app IDs for the apps to be described. If you use this parameter, @DescribeApps@ returns a description of the specified apps. Otherwise, it returns a description of every app.
daAppIds :: Lens' DescribeApps [Text]
daAppIds = lens _daAppIds (\ s a -> s{_daAppIds = a}) . _Default . _Coerce

-- | The app stack ID. If you use this parameter, @DescribeApps@ returns a description of the apps in the specified stack.
daStackId :: Lens' DescribeApps (Maybe Text)
daStackId = lens _daStackId (\ s a -> s{_daStackId = a})

instance AWSRequest DescribeApps where
        type Rs DescribeApps = DescribeAppsResponse
        request = postJSON opsWorks
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAppsResponse' <$>
                   (x .?> "Apps" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable DescribeApps where

instance NFData DescribeApps where

instance ToHeaders DescribeApps where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.DescribeApps" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeApps where
        toJSON DescribeApps'{..}
          = object
              (catMaybes
                 [("AppIds" .=) <$> _daAppIds,
                  ("StackId" .=) <$> _daStackId])

instance ToPath DescribeApps where
        toPath = const "/"

instance ToQuery DescribeApps where
        toQuery = const mempty

-- | Contains the response to a @DescribeApps@ request.
--
--
--
-- /See:/ 'describeAppsResponse' smart constructor.
data DescribeAppsResponse = DescribeAppsResponse'
  { _darsApps           :: !(Maybe [App])
  , _darsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeAppsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsApps' - An array of @App@ objects that describe the specified apps.
--
-- * 'darsResponseStatus' - -- | The response status code.
describeAppsResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAppsResponse
describeAppsResponse pResponseStatus_ =
  DescribeAppsResponse'
    {_darsApps = Nothing, _darsResponseStatus = pResponseStatus_}


-- | An array of @App@ objects that describe the specified apps.
darsApps :: Lens' DescribeAppsResponse [App]
darsApps = lens _darsApps (\ s a -> s{_darsApps = a}) . _Default . _Coerce

-- | -- | The response status code.
darsResponseStatus :: Lens' DescribeAppsResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a})

instance NFData DescribeAppsResponse where
