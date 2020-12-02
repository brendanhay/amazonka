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
-- Module      : Network.AWS.WAF.UpdateRegexMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'RegexMatchTuple' objects (filters) in a 'RegexMatchSet' . For each @RegexMatchSetUpdate@ object, you specify the following values:
--
--
--     * Whether to insert or delete the object from the array. If you want to change a @RegexMatchSetUpdate@ object, you delete the existing object and add a new one.
--
--     * The part of a web request that you want AWS WAF to inspectupdate, such as a query string or the value of the @User-Agent@ header.
--
--     * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .
--
--     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
--
--
-- For example, you can create a @RegexPatternSet@ that matches any requests with @User-Agent@ headers that contain the string @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
--
-- To create and configure a @RegexMatchSet@ , perform the following steps:
--
--     * Create a @RegexMatchSet.@ For more information, see 'CreateRegexMatchSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexMatchSet@ request.
--
--     * Submit an @UpdateRegexMatchSet@ request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the identifier of the @RegexPatternSet@ that contain the regular expression patters you want AWS WAF to watch for.
--
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAF.UpdateRegexMatchSet
    (
    -- * Creating a Request
      updateRegexMatchSet
    , UpdateRegexMatchSet
    -- * Request Lenses
    , urmsRegexMatchSetId
    , urmsUpdates
    , urmsChangeToken

    -- * Destructuring the Response
    , updateRegexMatchSetResponse
    , UpdateRegexMatchSetResponse
    -- * Response Lenses
    , urmsrsChangeToken
    , urmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'updateRegexMatchSet' smart constructor.
data UpdateRegexMatchSet = UpdateRegexMatchSet'
  { _urmsRegexMatchSetId :: !Text
  , _urmsUpdates         :: !(List1 RegexMatchSetUpdate)
  , _urmsChangeToken     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRegexMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urmsRegexMatchSetId' - The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to update. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- * 'urmsUpdates' - An array of @RegexMatchSetUpdate@ objects that you want to insert into or delete from a 'RegexMatchSet' . For more information, see 'RegexMatchTuple' .
--
-- * 'urmsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
updateRegexMatchSet
    :: Text -- ^ 'urmsRegexMatchSetId'
    -> NonEmpty RegexMatchSetUpdate -- ^ 'urmsUpdates'
    -> Text -- ^ 'urmsChangeToken'
    -> UpdateRegexMatchSet
updateRegexMatchSet pRegexMatchSetId_ pUpdates_ pChangeToken_ =
  UpdateRegexMatchSet'
    { _urmsRegexMatchSetId = pRegexMatchSetId_
    , _urmsUpdates = _List1 # pUpdates_
    , _urmsChangeToken = pChangeToken_
    }


-- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to update. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
urmsRegexMatchSetId :: Lens' UpdateRegexMatchSet Text
urmsRegexMatchSetId = lens _urmsRegexMatchSetId (\ s a -> s{_urmsRegexMatchSetId = a})

-- | An array of @RegexMatchSetUpdate@ objects that you want to insert into or delete from a 'RegexMatchSet' . For more information, see 'RegexMatchTuple' .
urmsUpdates :: Lens' UpdateRegexMatchSet (NonEmpty RegexMatchSetUpdate)
urmsUpdates = lens _urmsUpdates (\ s a -> s{_urmsUpdates = a}) . _List1

-- | The value returned by the most recent call to 'GetChangeToken' .
urmsChangeToken :: Lens' UpdateRegexMatchSet Text
urmsChangeToken = lens _urmsChangeToken (\ s a -> s{_urmsChangeToken = a})

instance AWSRequest UpdateRegexMatchSet where
        type Rs UpdateRegexMatchSet =
             UpdateRegexMatchSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRegexMatchSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateRegexMatchSet where

instance NFData UpdateRegexMatchSet where

instance ToHeaders UpdateRegexMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.UpdateRegexMatchSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRegexMatchSet where
        toJSON UpdateRegexMatchSet'{..}
          = object
              (catMaybes
                 [Just ("RegexMatchSetId" .= _urmsRegexMatchSetId),
                  Just ("Updates" .= _urmsUpdates),
                  Just ("ChangeToken" .= _urmsChangeToken)])

instance ToPath UpdateRegexMatchSet where
        toPath = const "/"

instance ToQuery UpdateRegexMatchSet where
        toQuery = const mempty

-- | /See:/ 'updateRegexMatchSetResponse' smart constructor.
data UpdateRegexMatchSetResponse = UpdateRegexMatchSetResponse'
  { _urmsrsChangeToken    :: !(Maybe Text)
  , _urmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRegexMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urmsrsChangeToken' - The @ChangeToken@ that you used to submit the @UpdateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'urmsrsResponseStatus' - -- | The response status code.
updateRegexMatchSetResponse
    :: Int -- ^ 'urmsrsResponseStatus'
    -> UpdateRegexMatchSetResponse
updateRegexMatchSetResponse pResponseStatus_ =
  UpdateRegexMatchSetResponse'
    {_urmsrsChangeToken = Nothing, _urmsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @UpdateRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
urmsrsChangeToken :: Lens' UpdateRegexMatchSetResponse (Maybe Text)
urmsrsChangeToken = lens _urmsrsChangeToken (\ s a -> s{_urmsrsChangeToken = a})

-- | -- | The response status code.
urmsrsResponseStatus :: Lens' UpdateRegexMatchSetResponse Int
urmsrsResponseStatus = lens _urmsrsResponseStatus (\ s a -> s{_urmsrsResponseStatus = a})

instance NFData UpdateRegexMatchSetResponse where
