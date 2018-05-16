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
-- Module      : Network.AWS.MQ.DescribeConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified configuration.
module Network.AWS.MQ.DescribeConfiguration
    (
    -- * Creating a Request
      describeConfiguration
    , DescribeConfiguration
    -- * Request Lenses
    , dcConfigurationId

    -- * Destructuring the Response
    , describeConfigurationResponse
    , DescribeConfigurationResponse
    -- * Response Lenses
    , dcrsEngineVersion
    , dcrsARN
    , dcrsLatestRevision
    , dcrsName
    , dcrsId
    , dcrsDescription
    , dcrsEngineType
    , dcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.MQ.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeConfiguration' smart constructor.
newtype DescribeConfiguration = DescribeConfiguration'
  { _dcConfigurationId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcConfigurationId' - The unique ID that Amazon MQ generates for the configuration.
describeConfiguration
    :: Text -- ^ 'dcConfigurationId'
    -> DescribeConfiguration
describeConfiguration pConfigurationId_ =
  DescribeConfiguration' {_dcConfigurationId = pConfigurationId_}


-- | The unique ID that Amazon MQ generates for the configuration.
dcConfigurationId :: Lens' DescribeConfiguration Text
dcConfigurationId = lens _dcConfigurationId (\ s a -> s{_dcConfigurationId = a})

instance AWSRequest DescribeConfiguration where
        type Rs DescribeConfiguration =
             DescribeConfigurationResponse
        request = get mq
        response
          = receiveJSON
              (\ s h x ->
                 DescribeConfigurationResponse' <$>
                   (x .?> "engineVersion") <*> (x .?> "arn") <*>
                     (x .?> "latestRevision")
                     <*> (x .?> "name")
                     <*> (x .?> "id")
                     <*> (x .?> "description")
                     <*> (x .?> "engineType")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeConfiguration where

instance NFData DescribeConfiguration where

instance ToHeaders DescribeConfiguration where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DescribeConfiguration where
        toPath DescribeConfiguration'{..}
          = mconcat
              ["/v1/configurations/", toBS _dcConfigurationId]

instance ToQuery DescribeConfiguration where
        toQuery = const mempty

-- | /See:/ 'describeConfigurationResponse' smart constructor.
data DescribeConfigurationResponse = DescribeConfigurationResponse'
  { _dcrsEngineVersion  :: !(Maybe Text)
  , _dcrsARN            :: !(Maybe Text)
  , _dcrsLatestRevision :: !(Maybe ConfigurationRevision)
  , _dcrsName           :: !(Maybe Text)
  , _dcrsId             :: !(Maybe Text)
  , _dcrsDescription    :: !(Maybe Text)
  , _dcrsEngineType     :: !(Maybe EngineType)
  , _dcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrsEngineVersion' - Required. The version of the broker engine.
--
-- * 'dcrsARN' - Required. The ARN of the configuration.
--
-- * 'dcrsLatestRevision' - Required. The latest revision of the configuration.
--
-- * 'dcrsName' - Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
--
-- * 'dcrsId' - Required. The unique ID that Amazon MQ generates for the configuration.
--
-- * 'dcrsDescription' - Required. The description of the configuration.
--
-- * 'dcrsEngineType' - Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
--
-- * 'dcrsResponseStatus' - -- | The response status code.
describeConfigurationResponse
    :: Int -- ^ 'dcrsResponseStatus'
    -> DescribeConfigurationResponse
describeConfigurationResponse pResponseStatus_ =
  DescribeConfigurationResponse'
    { _dcrsEngineVersion = Nothing
    , _dcrsARN = Nothing
    , _dcrsLatestRevision = Nothing
    , _dcrsName = Nothing
    , _dcrsId = Nothing
    , _dcrsDescription = Nothing
    , _dcrsEngineType = Nothing
    , _dcrsResponseStatus = pResponseStatus_
    }


-- | Required. The version of the broker engine.
dcrsEngineVersion :: Lens' DescribeConfigurationResponse (Maybe Text)
dcrsEngineVersion = lens _dcrsEngineVersion (\ s a -> s{_dcrsEngineVersion = a})

-- | Required. The ARN of the configuration.
dcrsARN :: Lens' DescribeConfigurationResponse (Maybe Text)
dcrsARN = lens _dcrsARN (\ s a -> s{_dcrsARN = a})

-- | Required. The latest revision of the configuration.
dcrsLatestRevision :: Lens' DescribeConfigurationResponse (Maybe ConfigurationRevision)
dcrsLatestRevision = lens _dcrsLatestRevision (\ s a -> s{_dcrsLatestRevision = a})

-- | Required. The name of the configuration. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 1-150 characters long.
dcrsName :: Lens' DescribeConfigurationResponse (Maybe Text)
dcrsName = lens _dcrsName (\ s a -> s{_dcrsName = a})

-- | Required. The unique ID that Amazon MQ generates for the configuration.
dcrsId :: Lens' DescribeConfigurationResponse (Maybe Text)
dcrsId = lens _dcrsId (\ s a -> s{_dcrsId = a})

-- | Required. The description of the configuration.
dcrsDescription :: Lens' DescribeConfigurationResponse (Maybe Text)
dcrsDescription = lens _dcrsDescription (\ s a -> s{_dcrsDescription = a})

-- | Required. The type of broker engine. Note: Currently, Amazon MQ supports only ACTIVEMQ.
dcrsEngineType :: Lens' DescribeConfigurationResponse (Maybe EngineType)
dcrsEngineType = lens _dcrsEngineType (\ s a -> s{_dcrsEngineType = a})

-- | -- | The response status code.
dcrsResponseStatus :: Lens' DescribeConfigurationResponse Int
dcrsResponseStatus = lens _dcrsResponseStatus (\ s a -> s{_dcrsResponseStatus = a})

instance NFData DescribeConfigurationResponse where
