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
-- Module      : Network.AWS.CloudWatchEvents.DescribeEventBus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the external AWS accounts that are permitted to write events to your account using your account's event bus, and the associated policy. To enable your account to receive events from other accounts, use 'PutPermission' .
--
--
module Network.AWS.CloudWatchEvents.DescribeEventBus
    (
    -- * Creating a Request
      describeEventBus
    , DescribeEventBus

    -- * Destructuring the Response
    , describeEventBusResponse
    , DescribeEventBusResponse
    -- * Response Lenses
    , debrsARN
    , debrsName
    , debrsPolicy
    , debrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventBus' smart constructor.
data DescribeEventBus =
  DescribeEventBus'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventBus' with the minimum fields required to make a request.
--
describeEventBus
    :: DescribeEventBus
describeEventBus = DescribeEventBus'


instance AWSRequest DescribeEventBus where
        type Rs DescribeEventBus = DescribeEventBusResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventBusResponse' <$>
                   (x .?> "Arn") <*> (x .?> "Name") <*> (x .?> "Policy")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEventBus where

instance NFData DescribeEventBus where

instance ToHeaders DescribeEventBus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.DescribeEventBus" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEventBus where
        toJSON = const (Object mempty)

instance ToPath DescribeEventBus where
        toPath = const "/"

instance ToQuery DescribeEventBus where
        toQuery = const mempty

-- | /See:/ 'describeEventBusResponse' smart constructor.
data DescribeEventBusResponse = DescribeEventBusResponse'
  { _debrsARN            :: !(Maybe Text)
  , _debrsName           :: !(Maybe Text)
  , _debrsPolicy         :: !(Maybe Text)
  , _debrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventBusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'debrsARN' - The Amazon Resource Name (ARN) of the account permitted to write events to the current account.
--
-- * 'debrsName' - The name of the event bus. Currently, this is always @default@ .
--
-- * 'debrsPolicy' - The policy that enables the external account to send events to your account.
--
-- * 'debrsResponseStatus' - -- | The response status code.
describeEventBusResponse
    :: Int -- ^ 'debrsResponseStatus'
    -> DescribeEventBusResponse
describeEventBusResponse pResponseStatus_ =
  DescribeEventBusResponse'
    { _debrsARN = Nothing
    , _debrsName = Nothing
    , _debrsPolicy = Nothing
    , _debrsResponseStatus = pResponseStatus_
    }


-- | The Amazon Resource Name (ARN) of the account permitted to write events to the current account.
debrsARN :: Lens' DescribeEventBusResponse (Maybe Text)
debrsARN = lens _debrsARN (\ s a -> s{_debrsARN = a})

-- | The name of the event bus. Currently, this is always @default@ .
debrsName :: Lens' DescribeEventBusResponse (Maybe Text)
debrsName = lens _debrsName (\ s a -> s{_debrsName = a})

-- | The policy that enables the external account to send events to your account.
debrsPolicy :: Lens' DescribeEventBusResponse (Maybe Text)
debrsPolicy = lens _debrsPolicy (\ s a -> s{_debrsPolicy = a})

-- | -- | The response status code.
debrsResponseStatus :: Lens' DescribeEventBusResponse Int
debrsResponseStatus = lens _debrsResponseStatus (\ s a -> s{_debrsResponseStatus = a})

instance NFData DescribeEventBusResponse where
