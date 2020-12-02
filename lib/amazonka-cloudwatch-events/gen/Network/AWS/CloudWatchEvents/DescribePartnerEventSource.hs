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
-- Module      : Network.AWS.CloudWatchEvents.DescribePartnerEventSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to list details about a partner event source that they have created.
--
--
module Network.AWS.CloudWatchEvents.DescribePartnerEventSource
    (
    -- * Creating a Request
      describePartnerEventSource
    , DescribePartnerEventSource
    -- * Request Lenses
    , dpespName

    -- * Destructuring the Response
    , describePartnerEventSourceResponse
    , DescribePartnerEventSourceResponse
    -- * Response Lenses
    , dpesrsARN
    , dpesrsName
    , dpesrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describePartnerEventSource' smart constructor.
newtype DescribePartnerEventSource = DescribePartnerEventSource'
  { _dpespName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePartnerEventSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpespName' - The name of the event source to display.
describePartnerEventSource
    :: Text -- ^ 'dpespName'
    -> DescribePartnerEventSource
describePartnerEventSource pName_ =
  DescribePartnerEventSource' {_dpespName = pName_}


-- | The name of the event source to display.
dpespName :: Lens' DescribePartnerEventSource Text
dpespName = lens _dpespName (\ s a -> s{_dpespName = a})

instance AWSRequest DescribePartnerEventSource where
        type Rs DescribePartnerEventSource =
             DescribePartnerEventSourceResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 DescribePartnerEventSourceResponse' <$>
                   (x .?> "Arn") <*> (x .?> "Name") <*>
                     (pure (fromEnum s)))

instance Hashable DescribePartnerEventSource where

instance NFData DescribePartnerEventSource where

instance ToHeaders DescribePartnerEventSource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.DescribePartnerEventSource" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribePartnerEventSource where
        toJSON DescribePartnerEventSource'{..}
          = object (catMaybes [Just ("Name" .= _dpespName)])

instance ToPath DescribePartnerEventSource where
        toPath = const "/"

instance ToQuery DescribePartnerEventSource where
        toQuery = const mempty

-- | /See:/ 'describePartnerEventSourceResponse' smart constructor.
data DescribePartnerEventSourceResponse = DescribePartnerEventSourceResponse'
  { _dpesrsARN            :: !(Maybe Text)
  , _dpesrsName           :: !(Maybe Text)
  , _dpesrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribePartnerEventSourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpesrsARN' - The ARN of the event source.
--
-- * 'dpesrsName' - The name of the event source.
--
-- * 'dpesrsResponseStatus' - -- | The response status code.
describePartnerEventSourceResponse
    :: Int -- ^ 'dpesrsResponseStatus'
    -> DescribePartnerEventSourceResponse
describePartnerEventSourceResponse pResponseStatus_ =
  DescribePartnerEventSourceResponse'
    { _dpesrsARN = Nothing
    , _dpesrsName = Nothing
    , _dpesrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the event source.
dpesrsARN :: Lens' DescribePartnerEventSourceResponse (Maybe Text)
dpesrsARN = lens _dpesrsARN (\ s a -> s{_dpesrsARN = a})

-- | The name of the event source.
dpesrsName :: Lens' DescribePartnerEventSourceResponse (Maybe Text)
dpesrsName = lens _dpesrsName (\ s a -> s{_dpesrsName = a})

-- | -- | The response status code.
dpesrsResponseStatus :: Lens' DescribePartnerEventSourceResponse Int
dpesrsResponseStatus = lens _dpesrsResponseStatus (\ s a -> s{_dpesrsResponseStatus = a})

instance NFData DescribePartnerEventSourceResponse
         where
