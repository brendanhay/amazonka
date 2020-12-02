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
-- Module      : Network.AWS.WAF.UpdateRegexPatternSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes @RegexPatternString@ objects in a 'RegexPatternSet' . For each @RegexPatternString@ object, you specify the following values:
--
--
--     * Whether to insert or delete the @RegexPatternString@ .
--
--     * The regular expression pattern that you want to insert or delete. For more information, see 'RegexPatternSet' .
--
--
--
-- For example, you can create a @RegexPatternString@ such as @B[a@]dB[o0]t@ . AWS WAF will match this @RegexPatternString@ to:
--
--     * BadBot
--
--     * BadB0t
--
--     * B@dBot
--
--     * B@dB0t
--
--
--
-- To create and configure a @RegexPatternSet@ , perform the following steps:
--
--     * Create a @RegexPatternSet.@ For more information, see 'CreateRegexPatternSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an @UpdateRegexPatternSet@ request.
--
--     * Submit an @UpdateRegexPatternSet@ request to specify the regular expression pattern that you want AWS WAF to watch for.
--
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAF.UpdateRegexPatternSet
    (
    -- * Creating a Request
      updateRegexPatternSet
    , UpdateRegexPatternSet
    -- * Request Lenses
    , urpsRegexPatternSetId
    , urpsUpdates
    , urpsChangeToken

    -- * Destructuring the Response
    , updateRegexPatternSetResponse
    , UpdateRegexPatternSetResponse
    -- * Response Lenses
    , urpsrsChangeToken
    , urpsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'updateRegexPatternSet' smart constructor.
data UpdateRegexPatternSet = UpdateRegexPatternSet'
  { _urpsRegexPatternSetId :: !Text
  , _urpsUpdates           :: !(List1 RegexPatternSetUpdate)
  , _urpsChangeToken       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRegexPatternSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urpsRegexPatternSetId' - The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to update. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- * 'urpsUpdates' - An array of @RegexPatternSetUpdate@ objects that you want to insert into or delete from a 'RegexPatternSet' .
--
-- * 'urpsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
updateRegexPatternSet
    :: Text -- ^ 'urpsRegexPatternSetId'
    -> NonEmpty RegexPatternSetUpdate -- ^ 'urpsUpdates'
    -> Text -- ^ 'urpsChangeToken'
    -> UpdateRegexPatternSet
updateRegexPatternSet pRegexPatternSetId_ pUpdates_ pChangeToken_ =
  UpdateRegexPatternSet'
    { _urpsRegexPatternSetId = pRegexPatternSetId_
    , _urpsUpdates = _List1 # pUpdates_
    , _urpsChangeToken = pChangeToken_
    }


-- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to update. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
urpsRegexPatternSetId :: Lens' UpdateRegexPatternSet Text
urpsRegexPatternSetId = lens _urpsRegexPatternSetId (\ s a -> s{_urpsRegexPatternSetId = a})

-- | An array of @RegexPatternSetUpdate@ objects that you want to insert into or delete from a 'RegexPatternSet' .
urpsUpdates :: Lens' UpdateRegexPatternSet (NonEmpty RegexPatternSetUpdate)
urpsUpdates = lens _urpsUpdates (\ s a -> s{_urpsUpdates = a}) . _List1

-- | The value returned by the most recent call to 'GetChangeToken' .
urpsChangeToken :: Lens' UpdateRegexPatternSet Text
urpsChangeToken = lens _urpsChangeToken (\ s a -> s{_urpsChangeToken = a})

instance AWSRequest UpdateRegexPatternSet where
        type Rs UpdateRegexPatternSet =
             UpdateRegexPatternSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 UpdateRegexPatternSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateRegexPatternSet where

instance NFData UpdateRegexPatternSet where

instance ToHeaders UpdateRegexPatternSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.UpdateRegexPatternSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateRegexPatternSet where
        toJSON UpdateRegexPatternSet'{..}
          = object
              (catMaybes
                 [Just
                    ("RegexPatternSetId" .= _urpsRegexPatternSetId),
                  Just ("Updates" .= _urpsUpdates),
                  Just ("ChangeToken" .= _urpsChangeToken)])

instance ToPath UpdateRegexPatternSet where
        toPath = const "/"

instance ToQuery UpdateRegexPatternSet where
        toQuery = const mempty

-- | /See:/ 'updateRegexPatternSetResponse' smart constructor.
data UpdateRegexPatternSetResponse = UpdateRegexPatternSetResponse'
  { _urpsrsChangeToken    :: !(Maybe Text)
  , _urpsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRegexPatternSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urpsrsChangeToken' - The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'urpsrsResponseStatus' - -- | The response status code.
updateRegexPatternSetResponse
    :: Int -- ^ 'urpsrsResponseStatus'
    -> UpdateRegexPatternSetResponse
updateRegexPatternSetResponse pResponseStatus_ =
  UpdateRegexPatternSetResponse'
    {_urpsrsChangeToken = Nothing, _urpsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @UpdateRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
urpsrsChangeToken :: Lens' UpdateRegexPatternSetResponse (Maybe Text)
urpsrsChangeToken = lens _urpsrsChangeToken (\ s a -> s{_urpsrsChangeToken = a})

-- | -- | The response status code.
urpsrsResponseStatus :: Lens' UpdateRegexPatternSetResponse Int
urpsrsResponseStatus = lens _urpsrsResponseStatus (\ s a -> s{_urpsrsResponseStatus = a})

instance NFData UpdateRegexPatternSetResponse where
