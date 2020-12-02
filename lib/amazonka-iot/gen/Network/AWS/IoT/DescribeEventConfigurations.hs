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
-- Module      : Network.AWS.IoT.DescribeEventConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes event configurations.
--
--
module Network.AWS.IoT.DescribeEventConfigurations
    (
    -- * Creating a Request
      describeEventConfigurations
    , DescribeEventConfigurations

    -- * Destructuring the Response
    , describeEventConfigurationsResponse
    , DescribeEventConfigurationsResponse
    -- * Response Lenses
    , decrsLastModifiedDate
    , decrsEventConfigurations
    , decrsCreationDate
    , decrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeEventConfigurations' smart constructor.
data DescribeEventConfigurations =
  DescribeEventConfigurations'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventConfigurations' with the minimum fields required to make a request.
--
describeEventConfigurations
    :: DescribeEventConfigurations
describeEventConfigurations = DescribeEventConfigurations'


instance AWSRequest DescribeEventConfigurations where
        type Rs DescribeEventConfigurations =
             DescribeEventConfigurationsResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEventConfigurationsResponse' <$>
                   (x .?> "lastModifiedDate") <*>
                     (x .?> "eventConfigurations" .!@ mempty)
                     <*> (x .?> "creationDate")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEventConfigurations where

instance NFData DescribeEventConfigurations where

instance ToHeaders DescribeEventConfigurations where
        toHeaders = const mempty

instance ToPath DescribeEventConfigurations where
        toPath = const "/event-configurations"

instance ToQuery DescribeEventConfigurations where
        toQuery = const mempty

-- | /See:/ 'describeEventConfigurationsResponse' smart constructor.
data DescribeEventConfigurationsResponse = DescribeEventConfigurationsResponse'
  { _decrsLastModifiedDate    :: !(Maybe POSIX)
  , _decrsEventConfigurations :: !(Maybe (Map EventType Configuration))
  , _decrsCreationDate        :: !(Maybe POSIX)
  , _decrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEventConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'decrsLastModifiedDate' - The date the event configurations were last modified.
--
-- * 'decrsEventConfigurations' - The event configurations.
--
-- * 'decrsCreationDate' - The creation date of the event configuration.
--
-- * 'decrsResponseStatus' - -- | The response status code.
describeEventConfigurationsResponse
    :: Int -- ^ 'decrsResponseStatus'
    -> DescribeEventConfigurationsResponse
describeEventConfigurationsResponse pResponseStatus_ =
  DescribeEventConfigurationsResponse'
    { _decrsLastModifiedDate = Nothing
    , _decrsEventConfigurations = Nothing
    , _decrsCreationDate = Nothing
    , _decrsResponseStatus = pResponseStatus_
    }


-- | The date the event configurations were last modified.
decrsLastModifiedDate :: Lens' DescribeEventConfigurationsResponse (Maybe UTCTime)
decrsLastModifiedDate = lens _decrsLastModifiedDate (\ s a -> s{_decrsLastModifiedDate = a}) . mapping _Time

-- | The event configurations.
decrsEventConfigurations :: Lens' DescribeEventConfigurationsResponse (HashMap EventType Configuration)
decrsEventConfigurations = lens _decrsEventConfigurations (\ s a -> s{_decrsEventConfigurations = a}) . _Default . _Map

-- | The creation date of the event configuration.
decrsCreationDate :: Lens' DescribeEventConfigurationsResponse (Maybe UTCTime)
decrsCreationDate = lens _decrsCreationDate (\ s a -> s{_decrsCreationDate = a}) . mapping _Time

-- | -- | The response status code.
decrsResponseStatus :: Lens' DescribeEventConfigurationsResponse Int
decrsResponseStatus = lens _decrsResponseStatus (\ s a -> s{_decrsResponseStatus = a})

instance NFData DescribeEventConfigurationsResponse
         where
