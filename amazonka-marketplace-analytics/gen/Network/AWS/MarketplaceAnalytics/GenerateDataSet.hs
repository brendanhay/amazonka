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
-- Module      : Network.AWS.MarketplaceAnalytics.GenerateDataSet
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a data set type and data set publication date, asynchronously
-- publishes the requested data set to the specified S3 bucket and notifies
-- the specified SNS topic once the data is available. Returns a unique
-- request identifier that can be used to correlate requests with
-- notifications from the SNS topic. Data sets will be published in
-- comma-separated values (CSV) format with the file name
-- {data_set_type}_YYYY-MM-DD.csv. If a file with the same name already
-- exists (e.g. if the same data set is requested twice), the original file
-- will be overwritten by the new file. Requires a Role with an attached
-- permissions policy providing Allow permissions for the following
-- actions: s3:PutObject, s3:getBucketLocation, sns:SetRegion,
-- sns:ListTopics, sns:Publish, iam:GetRolePolicy.
--
-- /See:/ <http://docs.aws.amazon.com/marketplace#GenerateDataSet.html AWS API Reference> for GenerateDataSet.
module Network.AWS.MarketplaceAnalytics.GenerateDataSet
    (
    -- * Creating a Request
      generateDataSet
    , GenerateDataSet
    -- * Request Lenses
    , gdsDestinationS3Prefix
    , gdsDataSetType
    , gdsDataSetPublicationDate
    , gdsRoleNameARN
    , gdsDestinationS3BucketName
    , gdsSnsTopicARN

    -- * Destructuring the Response
    , generateDataSetResponse
    , GenerateDataSetResponse
    -- * Response Lenses
    , gdsrsDataSetRequestId
    , gdsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.MarketplaceAnalytics.Types
import           Network.AWS.MarketplaceAnalytics.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the GenerateDataSet operation.
--
-- /See:/ 'generateDataSet' smart constructor.
data GenerateDataSet = GenerateDataSet'
    { _gdsDestinationS3Prefix     :: !(Maybe Text)
    , _gdsDataSetType             :: !DataSetType
    , _gdsDataSetPublicationDate  :: !POSIX
    , _gdsRoleNameARN             :: !Text
    , _gdsDestinationS3BucketName :: !Text
    , _gdsSnsTopicARN             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GenerateDataSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsDestinationS3Prefix'
--
-- * 'gdsDataSetType'
--
-- * 'gdsDataSetPublicationDate'
--
-- * 'gdsRoleNameARN'
--
-- * 'gdsDestinationS3BucketName'
--
-- * 'gdsSnsTopicARN'
generateDataSet
    :: DataSetType -- ^ 'gdsDataSetType'
    -> UTCTime -- ^ 'gdsDataSetPublicationDate'
    -> Text -- ^ 'gdsRoleNameARN'
    -> Text -- ^ 'gdsDestinationS3BucketName'
    -> Text -- ^ 'gdsSnsTopicARN'
    -> GenerateDataSet
generateDataSet pDataSetType_ pDataSetPublicationDate_ pRoleNameARN_ pDestinationS3BucketName_ pSnsTopicARN_ =
    GenerateDataSet'
    { _gdsDestinationS3Prefix = Nothing
    , _gdsDataSetType = pDataSetType_
    , _gdsDataSetPublicationDate = _Time # pDataSetPublicationDate_
    , _gdsRoleNameARN = pRoleNameARN_
    , _gdsDestinationS3BucketName = pDestinationS3BucketName_
    , _gdsSnsTopicARN = pSnsTopicARN_
    }

-- | Undocumented member.
gdsDestinationS3Prefix :: Lens' GenerateDataSet (Maybe Text)
gdsDestinationS3Prefix = lens _gdsDestinationS3Prefix (\ s a -> s{_gdsDestinationS3Prefix = a});

-- | Undocumented member.
gdsDataSetType :: Lens' GenerateDataSet DataSetType
gdsDataSetType = lens _gdsDataSetType (\ s a -> s{_gdsDataSetType = a});

-- | Undocumented member.
gdsDataSetPublicationDate :: Lens' GenerateDataSet UTCTime
gdsDataSetPublicationDate = lens _gdsDataSetPublicationDate (\ s a -> s{_gdsDataSetPublicationDate = a}) . _Time;

-- | Undocumented member.
gdsRoleNameARN :: Lens' GenerateDataSet Text
gdsRoleNameARN = lens _gdsRoleNameARN (\ s a -> s{_gdsRoleNameARN = a});

-- | Undocumented member.
gdsDestinationS3BucketName :: Lens' GenerateDataSet Text
gdsDestinationS3BucketName = lens _gdsDestinationS3BucketName (\ s a -> s{_gdsDestinationS3BucketName = a});

-- | Undocumented member.
gdsSnsTopicARN :: Lens' GenerateDataSet Text
gdsSnsTopicARN = lens _gdsSnsTopicARN (\ s a -> s{_gdsSnsTopicARN = a});

instance AWSRequest GenerateDataSet where
        type Rs GenerateDataSet = GenerateDataSetResponse
        request = postJSON marketplaceAnalytics
        response
          = receiveJSON
              (\ s h x ->
                 GenerateDataSetResponse' <$>
                   (x .?> "dataSetRequestId") <*> (pure (fromEnum s)))

instance ToHeaders GenerateDataSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MarketplaceCommerceAnalytics20150701.GenerateDataSet"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GenerateDataSet where
        toJSON GenerateDataSet'{..}
          = object
              (catMaybes
                 [("destinationS3Prefix" .=) <$>
                    _gdsDestinationS3Prefix,
                  Just ("dataSetType" .= _gdsDataSetType),
                  Just
                    ("dataSetPublicationDate" .=
                       _gdsDataSetPublicationDate),
                  Just ("roleNameArn" .= _gdsRoleNameARN),
                  Just
                    ("destinationS3BucketName" .=
                       _gdsDestinationS3BucketName),
                  Just ("snsTopicArn" .= _gdsSnsTopicARN)])

instance ToPath GenerateDataSet where
        toPath = const "/"

instance ToQuery GenerateDataSet where
        toQuery = const mempty

-- | Container for the result of the GenerateDataSet operation.
--
-- /See:/ 'generateDataSetResponse' smart constructor.
data GenerateDataSetResponse = GenerateDataSetResponse'
    { _gdsrsDataSetRequestId :: !(Maybe Text)
    , _gdsrsResponseStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GenerateDataSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdsrsDataSetRequestId'
--
-- * 'gdsrsResponseStatus'
generateDataSetResponse
    :: Int -- ^ 'gdsrsResponseStatus'
    -> GenerateDataSetResponse
generateDataSetResponse pResponseStatus_ =
    GenerateDataSetResponse'
    { _gdsrsDataSetRequestId = Nothing
    , _gdsrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
gdsrsDataSetRequestId :: Lens' GenerateDataSetResponse (Maybe Text)
gdsrsDataSetRequestId = lens _gdsrsDataSetRequestId (\ s a -> s{_gdsrsDataSetRequestId = a});

-- | The response status code.
gdsrsResponseStatus :: Lens' GenerateDataSetResponse Int
gdsrsResponseStatus = lens _gdsrsResponseStatus (\ s a -> s{_gdsrsResponseStatus = a});
