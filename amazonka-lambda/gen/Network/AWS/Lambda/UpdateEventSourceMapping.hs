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
-- Module      : Network.AWS.Lambda.UpdateEventSourceMapping
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can update an event source mapping. This is useful if you want to
-- change the parameters of the existing mapping without losing your
-- position in the stream. You can change which function will receive the
-- stream records, but to change the stream itself, you must create a new
-- mapping.
--
-- This operation requires permission for the
-- 'lambda:UpdateEventSourceMapping' action.
--
-- /See:/ <http://docs.aws.amazon.com/lambda/latest/dg/API_UpdateEventSourceMapping.html AWS API Reference> for UpdateEventSourceMapping.
module Network.AWS.Lambda.UpdateEventSourceMapping
    (
    -- * Creating a Request
      updateEventSourceMapping
    , UpdateEventSourceMapping
    -- * Request Lenses
    , uesmEnabled
    , uesmBatchSize
    , uesmFunctionName
    , uesmUUId

    -- * Destructuring the Response
    , eventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    -- * Response Lenses
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
import           Network.AWS.Lambda.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'updateEventSourceMapping' smart constructor.
data UpdateEventSourceMapping = UpdateEventSourceMapping'
    { _uesmEnabled      :: !(Maybe Bool)
    , _uesmBatchSize    :: !(Maybe Nat)
    , _uesmFunctionName :: !(Maybe Text)
    , _uesmUUId         :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateEventSourceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uesmEnabled'
--
-- * 'uesmBatchSize'
--
-- * 'uesmFunctionName'
--
-- * 'uesmUUId'
updateEventSourceMapping
    :: Text -- ^ 'uesmUUId'
    -> UpdateEventSourceMapping
updateEventSourceMapping pUUId_ =
    UpdateEventSourceMapping'
    { _uesmEnabled = Nothing
    , _uesmBatchSize = Nothing
    , _uesmFunctionName = Nothing
    , _uesmUUId = pUUId_
    }

-- | Specifies whether AWS Lambda should actively poll the stream or not. If
-- disabled, AWS Lambda will not poll the stream.
uesmEnabled :: Lens' UpdateEventSourceMapping (Maybe Bool)
uesmEnabled = lens _uesmEnabled (\ s a -> s{_uesmEnabled = a});

-- | The maximum number of stream records that can be sent to your Lambda
-- function for a single invocation.
uesmBatchSize :: Lens' UpdateEventSourceMapping (Maybe Natural)
uesmBatchSize = lens _uesmBatchSize (\ s a -> s{_uesmBatchSize = a}) . mapping _Nat;

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
uesmFunctionName :: Lens' UpdateEventSourceMapping (Maybe Text)
uesmFunctionName = lens _uesmFunctionName (\ s a -> s{_uesmFunctionName = a});

-- | The event source mapping identifier.
uesmUUId :: Lens' UpdateEventSourceMapping Text
uesmUUId = lens _uesmUUId (\ s a -> s{_uesmUUId = a});

instance AWSRequest UpdateEventSourceMapping where
        type Rs UpdateEventSourceMapping =
             EventSourceMappingConfiguration
        request = putJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders UpdateEventSourceMapping where
        toHeaders = const mempty

instance ToJSON UpdateEventSourceMapping where
        toJSON UpdateEventSourceMapping'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _uesmEnabled,
                  ("BatchSize" .=) <$> _uesmBatchSize,
                  ("FunctionName" .=) <$> _uesmFunctionName])

instance ToPath UpdateEventSourceMapping where
        toPath UpdateEventSourceMapping'{..}
          = mconcat
              ["/2015-03-31/event-source-mappings/",
               toBS _uesmUUId]

instance ToQuery UpdateEventSourceMapping where
        toQuery = const mempty
