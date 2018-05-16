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
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationOutput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an external destination to your Amazon Kinesis Analytics application.
--
--
-- If you want Amazon Kinesis Analytics to deliver data from an in-application stream within your application to an external destination (such as an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an Amazon Lambda function), you add the relevant configuration to your application using this operation. You can configure one or more outputs for your application. Each output configuration maps an in-application stream and an external destination.
--
-- You can use one of the output configurations to deliver data from your in-application error stream to an external destination so that you can analyze the errors. For conceptual information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Understanding Application Output (Destination)> .
--
-- Note that any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the 'DescribeApplication' operation to find the current application version.
--
-- For the limits on the number of application inputs and outputs you can configure, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
--
-- This operation requires permissions to perform the @kinesisanalytics:AddApplicationOutput@ action.
--
module Network.AWS.KinesisAnalytics.AddApplicationOutput
    (
    -- * Creating a Request
      addApplicationOutput
    , AddApplicationOutput
    -- * Request Lenses
    , aaoApplicationName
    , aaoCurrentApplicationVersionId
    , aaoOutput

    -- * Destructuring the Response
    , addApplicationOutputResponse
    , AddApplicationOutputResponse
    -- * Response Lenses
    , aaorsResponseStatus
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
-- /See:/ 'addApplicationOutput' smart constructor.
data AddApplicationOutput = AddApplicationOutput'
  { _aaoApplicationName             :: !Text
  , _aaoCurrentApplicationVersionId :: !Nat
  , _aaoOutput                      :: !Output
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddApplicationOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaoApplicationName' - Name of the application to which you want to add the output configuration.
--
-- * 'aaoCurrentApplicationVersionId' - Version of the application to which you want to add the output configuration. You can use the 'DescribeApplication' operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- * 'aaoOutput' - An array of objects, each describing one output configuration. In the output configuration, you specify the name of an in-application stream, a destination (that is, an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an Amazon Lambda function), and record the formation to use when writing to the destination.
addApplicationOutput
    :: Text -- ^ 'aaoApplicationName'
    -> Natural -- ^ 'aaoCurrentApplicationVersionId'
    -> Output -- ^ 'aaoOutput'
    -> AddApplicationOutput
addApplicationOutput pApplicationName_ pCurrentApplicationVersionId_ pOutput_ =
  AddApplicationOutput'
    { _aaoApplicationName = pApplicationName_
    , _aaoCurrentApplicationVersionId = _Nat # pCurrentApplicationVersionId_
    , _aaoOutput = pOutput_
    }


-- | Name of the application to which you want to add the output configuration.
aaoApplicationName :: Lens' AddApplicationOutput Text
aaoApplicationName = lens _aaoApplicationName (\ s a -> s{_aaoApplicationName = a})

-- | Version of the application to which you want to add the output configuration. You can use the 'DescribeApplication' operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
aaoCurrentApplicationVersionId :: Lens' AddApplicationOutput Natural
aaoCurrentApplicationVersionId = lens _aaoCurrentApplicationVersionId (\ s a -> s{_aaoCurrentApplicationVersionId = a}) . _Nat

-- | An array of objects, each describing one output configuration. In the output configuration, you specify the name of an in-application stream, a destination (that is, an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an Amazon Lambda function), and record the formation to use when writing to the destination.
aaoOutput :: Lens' AddApplicationOutput Output
aaoOutput = lens _aaoOutput (\ s a -> s{_aaoOutput = a})

instance AWSRequest AddApplicationOutput where
        type Rs AddApplicationOutput =
             AddApplicationOutputResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 AddApplicationOutputResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AddApplicationOutput where

instance NFData AddApplicationOutput where

instance ToHeaders AddApplicationOutput where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.AddApplicationOutput" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddApplicationOutput where
        toJSON AddApplicationOutput'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _aaoApplicationName),
                  Just
                    ("CurrentApplicationVersionId" .=
                       _aaoCurrentApplicationVersionId),
                  Just ("Output" .= _aaoOutput)])

instance ToPath AddApplicationOutput where
        toPath = const "/"

instance ToQuery AddApplicationOutput where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'addApplicationOutputResponse' smart constructor.
newtype AddApplicationOutputResponse = AddApplicationOutputResponse'
  { _aaorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddApplicationOutputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaorsResponseStatus' - -- | The response status code.
addApplicationOutputResponse
    :: Int -- ^ 'aaorsResponseStatus'
    -> AddApplicationOutputResponse
addApplicationOutputResponse pResponseStatus_ =
  AddApplicationOutputResponse' {_aaorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aaorsResponseStatus :: Lens' AddApplicationOutputResponse Int
aaorsResponseStatus = lens _aaorsResponseStatus (\ s a -> s{_aaorsResponseStatus = a})

instance NFData AddApplicationOutputResponse where
