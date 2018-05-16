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
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationReferenceDataSource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a reference data source to an existing application.
--
--
-- Amazon Kinesis Analytics reads reference data (that is, an Amazon S3 object) and creates an in-application table within your application. In the request, you provide the source (S3 bucket name and object key name), name of the in-application table to create, and the necessary mapping information that describes how data in Amazon S3 object maps to columns in the resulting in-application table.
--
-- For conceptual information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . For the limits on data sources you can add to your application, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
--
-- This operation requires permissions to perform the @kinesisanalytics:AddApplicationOutput@ action.
--
module Network.AWS.KinesisAnalytics.AddApplicationReferenceDataSource
    (
    -- * Creating a Request
      addApplicationReferenceDataSource
    , AddApplicationReferenceDataSource
    -- * Request Lenses
    , aardsApplicationName
    , aardsCurrentApplicationVersionId
    , aardsReferenceDataSource

    -- * Destructuring the Response
    , addApplicationReferenceDataSourceResponse
    , AddApplicationReferenceDataSourceResponse
    -- * Response Lenses
    , aardsrsResponseStatus
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'addApplicationReferenceDataSource' smart constructor.
data AddApplicationReferenceDataSource = AddApplicationReferenceDataSource'
  { _aardsApplicationName             :: !Text
  , _aardsCurrentApplicationVersionId :: !Nat
  , _aardsReferenceDataSource         :: !ReferenceDataSource
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddApplicationReferenceDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aardsApplicationName' - Name of an existing application.
--
-- * 'aardsCurrentApplicationVersionId' - Version of the application for which you are adding the reference data source. You can use the 'DescribeApplication' operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- * 'aardsReferenceDataSource' - The reference data source can be an object in your Amazon S3 bucket. Amazon Kinesis Analytics reads the object and copies the data into the in-application table that is created. You provide an S3 bucket, object key name, and the resulting in-application table that is created. You must also provide an IAM role with the necessary permissions that Amazon Kinesis Analytics can assume to read the object from your S3 bucket on your behalf.
addApplicationReferenceDataSource
    :: Text -- ^ 'aardsApplicationName'
    -> Natural -- ^ 'aardsCurrentApplicationVersionId'
    -> ReferenceDataSource -- ^ 'aardsReferenceDataSource'
    -> AddApplicationReferenceDataSource
addApplicationReferenceDataSource pApplicationName_ pCurrentApplicationVersionId_ pReferenceDataSource_ =
  AddApplicationReferenceDataSource'
    { _aardsApplicationName = pApplicationName_
    , _aardsCurrentApplicationVersionId = _Nat # pCurrentApplicationVersionId_
    , _aardsReferenceDataSource = pReferenceDataSource_
    }


-- | Name of an existing application.
aardsApplicationName :: Lens' AddApplicationReferenceDataSource Text
aardsApplicationName = lens _aardsApplicationName (\ s a -> s{_aardsApplicationName = a})

-- | Version of the application for which you are adding the reference data source. You can use the 'DescribeApplication' operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
aardsCurrentApplicationVersionId :: Lens' AddApplicationReferenceDataSource Natural
aardsCurrentApplicationVersionId = lens _aardsCurrentApplicationVersionId (\ s a -> s{_aardsCurrentApplicationVersionId = a}) . _Nat

-- | The reference data source can be an object in your Amazon S3 bucket. Amazon Kinesis Analytics reads the object and copies the data into the in-application table that is created. You provide an S3 bucket, object key name, and the resulting in-application table that is created. You must also provide an IAM role with the necessary permissions that Amazon Kinesis Analytics can assume to read the object from your S3 bucket on your behalf.
aardsReferenceDataSource :: Lens' AddApplicationReferenceDataSource ReferenceDataSource
aardsReferenceDataSource = lens _aardsReferenceDataSource (\ s a -> s{_aardsReferenceDataSource = a})

instance AWSRequest AddApplicationReferenceDataSource
         where
        type Rs AddApplicationReferenceDataSource =
             AddApplicationReferenceDataSourceResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 AddApplicationReferenceDataSourceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AddApplicationReferenceDataSource
         where

instance NFData AddApplicationReferenceDataSource
         where

instance ToHeaders AddApplicationReferenceDataSource
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.AddApplicationReferenceDataSource"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddApplicationReferenceDataSource
         where
        toJSON AddApplicationReferenceDataSource'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _aardsApplicationName),
                  Just
                    ("CurrentApplicationVersionId" .=
                       _aardsCurrentApplicationVersionId),
                  Just
                    ("ReferenceDataSource" .=
                       _aardsReferenceDataSource)])

instance ToPath AddApplicationReferenceDataSource
         where
        toPath = const "/"

instance ToQuery AddApplicationReferenceDataSource
         where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'addApplicationReferenceDataSourceResponse' smart constructor.
newtype AddApplicationReferenceDataSourceResponse = AddApplicationReferenceDataSourceResponse'
  { _aardsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddApplicationReferenceDataSourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aardsrsResponseStatus' - -- | The response status code.
addApplicationReferenceDataSourceResponse
    :: Int -- ^ 'aardsrsResponseStatus'
    -> AddApplicationReferenceDataSourceResponse
addApplicationReferenceDataSourceResponse pResponseStatus_ =
  AddApplicationReferenceDataSourceResponse'
    {_aardsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aardsrsResponseStatus :: Lens' AddApplicationReferenceDataSourceResponse Int
aardsrsResponseStatus = lens _aardsrsResponseStatus (\ s a -> s{_aardsrsResponseStatus = a})

instance NFData
           AddApplicationReferenceDataSourceResponse
         where
