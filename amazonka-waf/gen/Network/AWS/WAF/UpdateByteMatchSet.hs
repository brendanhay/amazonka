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
-- Module      : Network.AWS.WAF.UpdateByteMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'ByteMatchTuple' objects (filters) in a 'ByteMatchSet' . For each @ByteMatchTuple@ object, you specify the following values:
--
--
--     * Whether to insert or delete the object from the array. If you want to change a @ByteMatchSetUpdate@ object, you delete the existing object and add a new one.
--
--     * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header.
--
--     * The bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to look for. For more information, including how you specify the values for the AWS WAF API and the AWS CLI or SDKs, see @TargetString@ in the 'ByteMatchTuple' data type.
--
--     * Where to look, such as at the beginning or the end of a query string.
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
--
-- For example, you can add a @ByteMatchSetUpdate@ object that matches web requests in which @User-Agent@ headers contain the string @BadBot@ . You can then configure AWS WAF to block those requests.
--
-- To create and configure a @ByteMatchSet@ , perform the following steps:
--
--     * Create a @ByteMatchSet.@ For more information, see 'CreateByteMatchSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateByteMatchSet@ request.
--
--     * Submit an @UpdateByteMatchSet@ request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAF.UpdateByteMatchSet
    (
    -- * Creating a Request
      updateByteMatchSet
    , UpdateByteMatchSet
    -- * Request Lenses
    , ubmsByteMatchSetId
    , ubmsChangeToken
    , ubmsUpdates

    -- * Destructuring the Response
    , updateByteMatchSetResponse
    , UpdateByteMatchSetResponse
    -- * Response Lenses
    , ubmsrsChangeToken
    , ubmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'updateByteMatchSet' smart constructor.
data UpdateByteMatchSet = UpdateByteMatchSet'
  { _ubmsByteMatchSetId :: !Text
  , _ubmsChangeToken    :: !Text
  , _ubmsUpdates        :: !(List1 ByteMatchSetUpdate)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateByteMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubmsByteMatchSetId' - The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to update. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- * 'ubmsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
--
-- * 'ubmsUpdates' - An array of @ByteMatchSetUpdate@ objects that you want to insert into or delete from a 'ByteMatchSet' . For more information, see the applicable data types:     * 'ByteMatchSetUpdate' : Contains @Action@ and @ByteMatchTuple@      * 'ByteMatchTuple' : Contains @FieldToMatch@ , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@      * 'FieldToMatch' : Contains @Data@ and @Type@
updateByteMatchSet
    :: Text -- ^ 'ubmsByteMatchSetId'
    -> Text -- ^ 'ubmsChangeToken'
    -> NonEmpty ByteMatchSetUpdate -- ^ 'ubmsUpdates'
    -> UpdateByteMatchSet
updateByteMatchSet pByteMatchSetId_ pChangeToken_ pUpdates_ =
  UpdateByteMatchSet'
    { _ubmsByteMatchSetId = pByteMatchSetId_
    , _ubmsChangeToken = pChangeToken_
    , _ubmsUpdates = _List1 # pUpdates_
    }


-- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to update. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
ubmsByteMatchSetId :: Lens' UpdateByteMatchSet Text
ubmsByteMatchSetId = lens _ubmsByteMatchSetId (\ s a -> s{_ubmsByteMatchSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
ubmsChangeToken :: Lens' UpdateByteMatchSet Text
ubmsChangeToken = lens _ubmsChangeToken (\ s a -> s{_ubmsChangeToken = a})

-- | An array of @ByteMatchSetUpdate@ objects that you want to insert into or delete from a 'ByteMatchSet' . For more information, see the applicable data types:     * 'ByteMatchSetUpdate' : Contains @Action@ and @ByteMatchTuple@      * 'ByteMatchTuple' : Contains @FieldToMatch@ , @PositionalConstraint@ , @TargetString@ , and @TextTransformation@      * 'FieldToMatch' : Contains @Data@ and @Type@
ubmsUpdates :: Lens' UpdateByteMatchSet (NonEmpty ByteMatchSetUpdate)
ubmsUpdates = lens _ubmsUpdates (\ s a -> s{_ubmsUpdates = a}) . _List1

instance AWSRequest UpdateByteMatchSet where
        type Rs UpdateByteMatchSet =
             UpdateByteMatchSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 UpdateByteMatchSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateByteMatchSet where

instance NFData UpdateByteMatchSet where

instance ToHeaders UpdateByteMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.UpdateByteMatchSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateByteMatchSet where
        toJSON UpdateByteMatchSet'{..}
          = object
              (catMaybes
                 [Just ("ByteMatchSetId" .= _ubmsByteMatchSetId),
                  Just ("ChangeToken" .= _ubmsChangeToken),
                  Just ("Updates" .= _ubmsUpdates)])

instance ToPath UpdateByteMatchSet where
        toPath = const "/"

instance ToQuery UpdateByteMatchSet where
        toQuery = const mempty

-- | /See:/ 'updateByteMatchSetResponse' smart constructor.
data UpdateByteMatchSetResponse = UpdateByteMatchSetResponse'
  { _ubmsrsChangeToken    :: !(Maybe Text)
  , _ubmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateByteMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubmsrsChangeToken' - The @ChangeToken@ that you used to submit the @UpdateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'ubmsrsResponseStatus' - -- | The response status code.
updateByteMatchSetResponse
    :: Int -- ^ 'ubmsrsResponseStatus'
    -> UpdateByteMatchSetResponse
updateByteMatchSetResponse pResponseStatus_ =
  UpdateByteMatchSetResponse'
    {_ubmsrsChangeToken = Nothing, _ubmsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @UpdateByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
ubmsrsChangeToken :: Lens' UpdateByteMatchSetResponse (Maybe Text)
ubmsrsChangeToken = lens _ubmsrsChangeToken (\ s a -> s{_ubmsrsChangeToken = a})

-- | -- | The response status code.
ubmsrsResponseStatus :: Lens' UpdateByteMatchSetResponse Int
ubmsrsResponseStatus = lens _ubmsrsResponseStatus (\ s a -> s{_ubmsrsResponseStatus = a})

instance NFData UpdateByteMatchSetResponse where
