{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.UpdateEventSourceMapping
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- You can update an event source mapping. This is useful if you want to
-- change the parameters of the existing mapping without losing your
-- position in the stream. You can change which function will receive the
-- stream records, but to change the stream itself, you must create a new
-- mapping.
--
-- This operation requires permission for the
-- @lambda:UpdateEventSourceMapping@ action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateEventSourceMapping.html>
module Network.AWS.Lambda.UpdateEventSourceMapping
    (
    -- * Request
      UpdateEventSourceMapping
    -- ** Request constructor
    , updateEventSourceMapping
    -- ** Request lenses
    , uesmrqEnabled
    , uesmrqBatchSize
    , uesmrqFunctionName
    , uesmrqUUId

    -- * Response
    , EventSourceMappingConfiguration
    -- ** Response constructor
    , eventSourceMappingConfiguration
    -- ** Response lenses
    , esmcEventSourceARN
    , esmcFunctionARN
    , esmcState
    , esmcUUId
    , esmcLastProcessingResult
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcLastModified
    ) where

import           Network.AWS.Lambda.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateEventSourceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uesmrqEnabled'
--
-- * 'uesmrqBatchSize'
--
-- * 'uesmrqFunctionName'
--
-- * 'uesmrqUUId'
data UpdateEventSourceMapping = UpdateEventSourceMapping'
    { _uesmrqEnabled      :: !(Maybe Bool)
    , _uesmrqBatchSize    :: !(Maybe Nat)
    , _uesmrqFunctionName :: !(Maybe Text)
    , _uesmrqUUId         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateEventSourceMapping' smart constructor.
updateEventSourceMapping :: Text -> UpdateEventSourceMapping
updateEventSourceMapping pUUId =
    UpdateEventSourceMapping'
    { _uesmrqEnabled = Nothing
    , _uesmrqBatchSize = Nothing
    , _uesmrqFunctionName = Nothing
    , _uesmrqUUId = pUUId
    }

-- | Specifies whether AWS Lambda should actively poll the stream or not. If
-- disabled, AWS Lambda will not poll the stream.
uesmrqEnabled :: Lens' UpdateEventSourceMapping (Maybe Bool)
uesmrqEnabled = lens _uesmrqEnabled (\ s a -> s{_uesmrqEnabled = a});

-- | The maximum number of stream records that can be sent to your Lambda
-- function for a single invocation.
uesmrqBatchSize :: Lens' UpdateEventSourceMapping (Maybe Natural)
uesmrqBatchSize = lens _uesmrqBatchSize (\ s a -> s{_uesmrqBatchSize = a}) . mapping _Nat;

-- | The Lambda function to which you want the stream records sent.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
uesmrqFunctionName :: Lens' UpdateEventSourceMapping (Maybe Text)
uesmrqFunctionName = lens _uesmrqFunctionName (\ s a -> s{_uesmrqFunctionName = a});

-- | The event source mapping identifier.
uesmrqUUId :: Lens' UpdateEventSourceMapping Text
uesmrqUUId = lens _uesmrqUUId (\ s a -> s{_uesmrqUUId = a});

instance AWSRequest UpdateEventSourceMapping where
        type Sv UpdateEventSourceMapping = Lambda
        type Rs UpdateEventSourceMapping =
             EventSourceMappingConfiguration
        request = putJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders UpdateEventSourceMapping where
        toHeaders = const mempty

instance ToJSON UpdateEventSourceMapping where
        toJSON UpdateEventSourceMapping'{..}
          = object
              ["Enabled" .= _uesmrqEnabled,
               "BatchSize" .= _uesmrqBatchSize,
               "FunctionName" .= _uesmrqFunctionName]

instance ToPath UpdateEventSourceMapping where
        toPath UpdateEventSourceMapping'{..}
          = mconcat
              ["/2015-03-31/event-source-mappings/",
               toText _uesmrqUUId]

instance ToQuery UpdateEventSourceMapping where
        toQuery = const mempty
