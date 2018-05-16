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
-- Module      : Network.AWS.Lambda.ListEventSourceMappings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of event source mappings you created using the @CreateEventSourceMapping@ (see 'CreateEventSourceMapping' ).
--
--
-- For each mapping, the API returns configuration information. You can optionally specify filters to retrieve specific event source mappings.
--
-- If you are using the versioning feature, you can get list of event source mappings for a specific Lambda function version or an alias as described in the @FunctionName@ parameter. For information about the versioning feature, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- This operation requires permission for the @lambda:ListEventSourceMappings@ action.
--
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListEventSourceMappings
    (
    -- * Creating a Request
      listEventSourceMappings
    , ListEventSourceMappings
    -- * Request Lenses
    , lesmEventSourceARN
    , lesmMarker
    , lesmMaxItems
    , lesmFunctionName

    -- * Destructuring the Response
    , listEventSourceMappingsResponse
    , ListEventSourceMappingsResponse
    -- * Response Lenses
    , lesmrsEventSourceMappings
    , lesmrsNextMarker
    , lesmrsResponseStatus
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'listEventSourceMappings' smart constructor.
data ListEventSourceMappings = ListEventSourceMappings'
  { _lesmEventSourceARN :: !(Maybe Text)
  , _lesmMarker         :: !(Maybe Text)
  , _lesmMaxItems       :: !(Maybe Nat)
  , _lesmFunctionName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEventSourceMappings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lesmEventSourceARN' - The Amazon Resource Name (ARN) of the Amazon Kinesis stream. (This parameter is optional.)
--
-- * 'lesmMarker' - Optional string. An opaque pagination token returned from a previous @ListEventSourceMappings@ operation. If present, specifies to continue the list from where the returning call left off.
--
-- * 'lesmMaxItems' - Optional integer. Specifies the maximum number of event sources to return in response. This value must be greater than 0.
--
-- * 'lesmFunctionName' - The name of the Lambda function. You can specify the function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). If you are using versioning, you can also provide a qualified function ARN (ARN that is qualified with function version or alias name as suffix). AWS Lambda also allows you to specify only the function name with the account ID qualifier (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
listEventSourceMappings
    :: ListEventSourceMappings
listEventSourceMappings =
  ListEventSourceMappings'
    { _lesmEventSourceARN = Nothing
    , _lesmMarker = Nothing
    , _lesmMaxItems = Nothing
    , _lesmFunctionName = Nothing
    }


-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream. (This parameter is optional.)
lesmEventSourceARN :: Lens' ListEventSourceMappings (Maybe Text)
lesmEventSourceARN = lens _lesmEventSourceARN (\ s a -> s{_lesmEventSourceARN = a})

-- | Optional string. An opaque pagination token returned from a previous @ListEventSourceMappings@ operation. If present, specifies to continue the list from where the returning call left off.
lesmMarker :: Lens' ListEventSourceMappings (Maybe Text)
lesmMarker = lens _lesmMarker (\ s a -> s{_lesmMarker = a})

-- | Optional integer. Specifies the maximum number of event sources to return in response. This value must be greater than 0.
lesmMaxItems :: Lens' ListEventSourceMappings (Maybe Natural)
lesmMaxItems = lens _lesmMaxItems (\ s a -> s{_lesmMaxItems = a}) . mapping _Nat

-- | The name of the Lambda function. You can specify the function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). If you are using versioning, you can also provide a qualified function ARN (ARN that is qualified with function version or alias name as suffix). AWS Lambda also allows you to specify only the function name with the account ID qualifier (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
lesmFunctionName :: Lens' ListEventSourceMappings (Maybe Text)
lesmFunctionName = lens _lesmFunctionName (\ s a -> s{_lesmFunctionName = a})

instance AWSPager ListEventSourceMappings where
        page rq rs
          | stop (rs ^. lesmrsNextMarker) = Nothing
          | stop (rs ^. lesmrsEventSourceMappings) = Nothing
          | otherwise =
            Just $ rq & lesmMarker .~ rs ^. lesmrsNextMarker

instance AWSRequest ListEventSourceMappings where
        type Rs ListEventSourceMappings =
             ListEventSourceMappingsResponse
        request = get lambda
        response
          = receiveJSON
              (\ s h x ->
                 ListEventSourceMappingsResponse' <$>
                   (x .?> "EventSourceMappings" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListEventSourceMappings where

instance NFData ListEventSourceMappings where

instance ToHeaders ListEventSourceMappings where
        toHeaders = const mempty

instance ToPath ListEventSourceMappings where
        toPath = const "/2015-03-31/event-source-mappings/"

instance ToQuery ListEventSourceMappings where
        toQuery ListEventSourceMappings'{..}
          = mconcat
              ["EventSourceArn" =: _lesmEventSourceARN,
               "Marker" =: _lesmMarker, "MaxItems" =: _lesmMaxItems,
               "FunctionName" =: _lesmFunctionName]

-- | Contains a list of event sources (see 'EventSourceMappingConfiguration' )
--
--
--
-- /See:/ 'listEventSourceMappingsResponse' smart constructor.
data ListEventSourceMappingsResponse = ListEventSourceMappingsResponse'
  { _lesmrsEventSourceMappings :: !(Maybe [EventSourceMappingConfiguration])
  , _lesmrsNextMarker          :: !(Maybe Text)
  , _lesmrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListEventSourceMappingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lesmrsEventSourceMappings' - An array of @EventSourceMappingConfiguration@ objects.
--
-- * 'lesmrsNextMarker' - A string, present if there are more event source mappings.
--
-- * 'lesmrsResponseStatus' - -- | The response status code.
listEventSourceMappingsResponse
    :: Int -- ^ 'lesmrsResponseStatus'
    -> ListEventSourceMappingsResponse
listEventSourceMappingsResponse pResponseStatus_ =
  ListEventSourceMappingsResponse'
    { _lesmrsEventSourceMappings = Nothing
    , _lesmrsNextMarker = Nothing
    , _lesmrsResponseStatus = pResponseStatus_
    }


-- | An array of @EventSourceMappingConfiguration@ objects.
lesmrsEventSourceMappings :: Lens' ListEventSourceMappingsResponse [EventSourceMappingConfiguration]
lesmrsEventSourceMappings = lens _lesmrsEventSourceMappings (\ s a -> s{_lesmrsEventSourceMappings = a}) . _Default . _Coerce

-- | A string, present if there are more event source mappings.
lesmrsNextMarker :: Lens' ListEventSourceMappingsResponse (Maybe Text)
lesmrsNextMarker = lens _lesmrsNextMarker (\ s a -> s{_lesmrsNextMarker = a})

-- | -- | The response status code.
lesmrsResponseStatus :: Lens' ListEventSourceMappingsResponse Int
lesmrsResponseStatus = lens _lesmrsResponseStatus (\ s a -> s{_lesmrsResponseStatus = a})

instance NFData ListEventSourceMappingsResponse where
