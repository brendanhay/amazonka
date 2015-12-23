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
-- Module      : Network.AWS.IoT.DescribeThing
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified thing.
--
-- /See:/ <https://aws.amazon.com/iot#DescribeThing.html AWS API Reference> for DescribeThing.
module Network.AWS.IoT.DescribeThing
    (
    -- * Creating a Request
      describeThing
    , DescribeThing
    -- * Request Lenses
    , dThingName

    -- * Destructuring the Response
    , describeThingResponse
    , DescribeThingResponse
    -- * Response Lenses
    , dtrsDefaultClientId
    , dtrsAttributes
    , dtrsThingName
    , dtrsResponseStatus
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DescribeThing operation.
--
-- /See:/ 'describeThing' smart constructor.
newtype DescribeThing = DescribeThing'
    { _dThingName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeThing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dThingName'
describeThing
    :: Text -- ^ 'dThingName'
    -> DescribeThing
describeThing pThingName_ =
    DescribeThing'
    { _dThingName = pThingName_
    }

-- | The name of the thing.
dThingName :: Lens' DescribeThing Text
dThingName = lens _dThingName (\ s a -> s{_dThingName = a});

instance AWSRequest DescribeThing where
        type Rs DescribeThing = DescribeThingResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeThingResponse' <$>
                   (x .?> "defaultClientId") <*>
                     (x .?> "attributes" .!@ mempty)
                     <*> (x .?> "thingName")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeThing where
        toHeaders = const mempty

instance ToPath DescribeThing where
        toPath DescribeThing'{..}
          = mconcat ["/things/", toBS _dThingName]

instance ToQuery DescribeThing where
        toQuery = const mempty

-- | The output from the DescribeThing operation.
--
-- /See:/ 'describeThingResponse' smart constructor.
data DescribeThingResponse = DescribeThingResponse'
    { _dtrsDefaultClientId :: !(Maybe Text)
    , _dtrsAttributes      :: !(Maybe (Map Text Text))
    , _dtrsThingName       :: !(Maybe Text)
    , _dtrsResponseStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeThingResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsDefaultClientId'
--
-- * 'dtrsAttributes'
--
-- * 'dtrsThingName'
--
-- * 'dtrsResponseStatus'
describeThingResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DescribeThingResponse
describeThingResponse pResponseStatus_ =
    DescribeThingResponse'
    { _dtrsDefaultClientId = Nothing
    , _dtrsAttributes = Nothing
    , _dtrsThingName = Nothing
    , _dtrsResponseStatus = pResponseStatus_
    }

-- | The default client ID.
dtrsDefaultClientId :: Lens' DescribeThingResponse (Maybe Text)
dtrsDefaultClientId = lens _dtrsDefaultClientId (\ s a -> s{_dtrsDefaultClientId = a});

-- | The attributes which are name\/value pairs in JSON format. For example:
--
-- {\\\"attributes\\\":{\\\"some-name1\\\":\\\"some-value1\\\"},
-- {\\\"some-name2\\\":\\\"some-value2\\\"},
-- {\\\"some-name3\\\":\\\"some-value3\\\"}}
dtrsAttributes :: Lens' DescribeThingResponse (HashMap Text Text)
dtrsAttributes = lens _dtrsAttributes (\ s a -> s{_dtrsAttributes = a}) . _Default . _Map;

-- | The name of the thing.
dtrsThingName :: Lens' DescribeThingResponse (Maybe Text)
dtrsThingName = lens _dtrsThingName (\ s a -> s{_dtrsThingName = a});

-- | The response status code.
dtrsResponseStatus :: Lens' DescribeThingResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a});
