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
-- Module      : Network.AWS.SageMaker.DescribeSubscribedWorkteam
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a work team provided by a vendor. It returns details about the subscription with a vendor in the AWS Marketplace.
--
--
module Network.AWS.SageMaker.DescribeSubscribedWorkteam
    (
    -- * Creating a Request
      describeSubscribedWorkteam
    , DescribeSubscribedWorkteam
    -- * Request Lenses
    , dswWorkteamARN

    -- * Destructuring the Response
    , describeSubscribedWorkteamResponse
    , DescribeSubscribedWorkteamResponse
    -- * Response Lenses
    , dswrsResponseStatus
    , dswrsSubscribedWorkteam
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeSubscribedWorkteam' smart constructor.
newtype DescribeSubscribedWorkteam = DescribeSubscribedWorkteam'
  { _dswWorkteamARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSubscribedWorkteam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dswWorkteamARN' - The Amazon Resource Name (ARN) of the subscribed work team to describe.
describeSubscribedWorkteam
    :: Text -- ^ 'dswWorkteamARN'
    -> DescribeSubscribedWorkteam
describeSubscribedWorkteam pWorkteamARN_ =
  DescribeSubscribedWorkteam' {_dswWorkteamARN = pWorkteamARN_}


-- | The Amazon Resource Name (ARN) of the subscribed work team to describe.
dswWorkteamARN :: Lens' DescribeSubscribedWorkteam Text
dswWorkteamARN = lens _dswWorkteamARN (\ s a -> s{_dswWorkteamARN = a})

instance AWSRequest DescribeSubscribedWorkteam where
        type Rs DescribeSubscribedWorkteam =
             DescribeSubscribedWorkteamResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSubscribedWorkteamResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "SubscribedWorkteam"))

instance Hashable DescribeSubscribedWorkteam where

instance NFData DescribeSubscribedWorkteam where

instance ToHeaders DescribeSubscribedWorkteam where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeSubscribedWorkteam" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSubscribedWorkteam where
        toJSON DescribeSubscribedWorkteam'{..}
          = object
              (catMaybes [Just ("WorkteamArn" .= _dswWorkteamARN)])

instance ToPath DescribeSubscribedWorkteam where
        toPath = const "/"

instance ToQuery DescribeSubscribedWorkteam where
        toQuery = const mempty

-- | /See:/ 'describeSubscribedWorkteamResponse' smart constructor.
data DescribeSubscribedWorkteamResponse = DescribeSubscribedWorkteamResponse'
  { _dswrsResponseStatus     :: !Int
  , _dswrsSubscribedWorkteam :: !SubscribedWorkteam
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSubscribedWorkteamResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dswrsResponseStatus' - -- | The response status code.
--
-- * 'dswrsSubscribedWorkteam' - A @Workteam@ instance that contains information about the work team.
describeSubscribedWorkteamResponse
    :: Int -- ^ 'dswrsResponseStatus'
    -> SubscribedWorkteam -- ^ 'dswrsSubscribedWorkteam'
    -> DescribeSubscribedWorkteamResponse
describeSubscribedWorkteamResponse pResponseStatus_ pSubscribedWorkteam_ =
  DescribeSubscribedWorkteamResponse'
    { _dswrsResponseStatus = pResponseStatus_
    , _dswrsSubscribedWorkteam = pSubscribedWorkteam_
    }


-- | -- | The response status code.
dswrsResponseStatus :: Lens' DescribeSubscribedWorkteamResponse Int
dswrsResponseStatus = lens _dswrsResponseStatus (\ s a -> s{_dswrsResponseStatus = a})

-- | A @Workteam@ instance that contains information about the work team.
dswrsSubscribedWorkteam :: Lens' DescribeSubscribedWorkteamResponse SubscribedWorkteam
dswrsSubscribedWorkteam = lens _dswrsSubscribedWorkteam (\ s a -> s{_dswrsSubscribedWorkteam = a})

instance NFData DescribeSubscribedWorkteamResponse
         where
