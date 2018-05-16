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
-- Module      : Network.AWS.SageMaker.DescribeEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of an endpoint.
--
--
module Network.AWS.SageMaker.DescribeEndpoint
    (
    -- * Creating a Request
      describeEndpoint
    , DescribeEndpoint
    -- * Request Lenses
    , dEndpointName

    -- * Destructuring the Response
    , describeEndpointResponse
    , DescribeEndpointResponse
    -- * Response Lenses
    , dersFailureReason
    , dersProductionVariants
    , dersResponseStatus
    , dersEndpointName
    , dersEndpointARN
    , dersEndpointConfigName
    , dersEndpointStatus
    , dersCreationTime
    , dersLastModifiedTime
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeEndpoint' smart constructor.
newtype DescribeEndpoint = DescribeEndpoint'
  { _dEndpointName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dEndpointName' - The name of the endpoint.
describeEndpoint
    :: Text -- ^ 'dEndpointName'
    -> DescribeEndpoint
describeEndpoint pEndpointName_ =
  DescribeEndpoint' {_dEndpointName = pEndpointName_}


-- | The name of the endpoint.
dEndpointName :: Lens' DescribeEndpoint Text
dEndpointName = lens _dEndpointName (\ s a -> s{_dEndpointName = a})

instance AWSRequest DescribeEndpoint where
        type Rs DescribeEndpoint = DescribeEndpointResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeEndpointResponse' <$>
                   (x .?> "FailureReason") <*>
                     (x .?> "ProductionVariants")
                     <*> (pure (fromEnum s))
                     <*> (x .:> "EndpointName")
                     <*> (x .:> "EndpointArn")
                     <*> (x .:> "EndpointConfigName")
                     <*> (x .:> "EndpointStatus")
                     <*> (x .:> "CreationTime")
                     <*> (x .:> "LastModifiedTime"))

instance Hashable DescribeEndpoint where

instance NFData DescribeEndpoint where

instance ToHeaders DescribeEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeEndpoint where
        toJSON DescribeEndpoint'{..}
          = object
              (catMaybes [Just ("EndpointName" .= _dEndpointName)])

instance ToPath DescribeEndpoint where
        toPath = const "/"

instance ToQuery DescribeEndpoint where
        toQuery = const mempty

-- | /See:/ 'describeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { _dersFailureReason      :: !(Maybe Text)
  , _dersProductionVariants :: !(Maybe (List1 ProductionVariantSummary))
  , _dersResponseStatus     :: !Int
  , _dersEndpointName       :: !Text
  , _dersEndpointARN        :: !Text
  , _dersEndpointConfigName :: !Text
  , _dersEndpointStatus     :: !EndpointStatus
  , _dersCreationTime       :: !POSIX
  , _dersLastModifiedTime   :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersFailureReason' - If the status of the endpoint is @Failed@ , the reason why it failed.
--
-- * 'dersProductionVariants' - An array of ProductionVariant objects, one for each model hosted behind this endpoint.
--
-- * 'dersResponseStatus' - -- | The response status code.
--
-- * 'dersEndpointName' - Name of the endpoint.
--
-- * 'dersEndpointARN' - The Amazon Resource Name (ARN) of the endpoint.
--
-- * 'dersEndpointConfigName' - The name of the endpoint configuration associated with this endpoint.
--
-- * 'dersEndpointStatus' - The status of the endpoint.
--
-- * 'dersCreationTime' - A timestamp that shows when the endpoint was created.
--
-- * 'dersLastModifiedTime' - A timestamp that shows when the endpoint was last modified.
describeEndpointResponse
    :: Int -- ^ 'dersResponseStatus'
    -> Text -- ^ 'dersEndpointName'
    -> Text -- ^ 'dersEndpointARN'
    -> Text -- ^ 'dersEndpointConfigName'
    -> EndpointStatus -- ^ 'dersEndpointStatus'
    -> UTCTime -- ^ 'dersCreationTime'
    -> UTCTime -- ^ 'dersLastModifiedTime'
    -> DescribeEndpointResponse
describeEndpointResponse pResponseStatus_ pEndpointName_ pEndpointARN_ pEndpointConfigName_ pEndpointStatus_ pCreationTime_ pLastModifiedTime_ =
  DescribeEndpointResponse'
    { _dersFailureReason = Nothing
    , _dersProductionVariants = Nothing
    , _dersResponseStatus = pResponseStatus_
    , _dersEndpointName = pEndpointName_
    , _dersEndpointARN = pEndpointARN_
    , _dersEndpointConfigName = pEndpointConfigName_
    , _dersEndpointStatus = pEndpointStatus_
    , _dersCreationTime = _Time # pCreationTime_
    , _dersLastModifiedTime = _Time # pLastModifiedTime_
    }


-- | If the status of the endpoint is @Failed@ , the reason why it failed.
dersFailureReason :: Lens' DescribeEndpointResponse (Maybe Text)
dersFailureReason = lens _dersFailureReason (\ s a -> s{_dersFailureReason = a})

-- | An array of ProductionVariant objects, one for each model hosted behind this endpoint.
dersProductionVariants :: Lens' DescribeEndpointResponse (Maybe (NonEmpty ProductionVariantSummary))
dersProductionVariants = lens _dersProductionVariants (\ s a -> s{_dersProductionVariants = a}) . mapping _List1

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeEndpointResponse Int
dersResponseStatus = lens _dersResponseStatus (\ s a -> s{_dersResponseStatus = a})

-- | Name of the endpoint.
dersEndpointName :: Lens' DescribeEndpointResponse Text
dersEndpointName = lens _dersEndpointName (\ s a -> s{_dersEndpointName = a})

-- | The Amazon Resource Name (ARN) of the endpoint.
dersEndpointARN :: Lens' DescribeEndpointResponse Text
dersEndpointARN = lens _dersEndpointARN (\ s a -> s{_dersEndpointARN = a})

-- | The name of the endpoint configuration associated with this endpoint.
dersEndpointConfigName :: Lens' DescribeEndpointResponse Text
dersEndpointConfigName = lens _dersEndpointConfigName (\ s a -> s{_dersEndpointConfigName = a})

-- | The status of the endpoint.
dersEndpointStatus :: Lens' DescribeEndpointResponse EndpointStatus
dersEndpointStatus = lens _dersEndpointStatus (\ s a -> s{_dersEndpointStatus = a})

-- | A timestamp that shows when the endpoint was created.
dersCreationTime :: Lens' DescribeEndpointResponse UTCTime
dersCreationTime = lens _dersCreationTime (\ s a -> s{_dersCreationTime = a}) . _Time

-- | A timestamp that shows when the endpoint was last modified.
dersLastModifiedTime :: Lens' DescribeEndpointResponse UTCTime
dersLastModifiedTime = lens _dersLastModifiedTime (\ s a -> s{_dersLastModifiedTime = a}) . _Time

instance NFData DescribeEndpointResponse where
