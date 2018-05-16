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
-- Module      : Network.AWS.WAFRegional.DeleteRegexMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'RegexMatchSet' . You can't delete a @RegexMatchSet@ if it's still used in any @Rules@ or if it still includes any @RegexMatchTuples@ objects (any filters).
--
--
-- If you just want to remove a @RegexMatchSet@ from a @Rule@ , use 'UpdateRule' .
--
-- To permanently delete a @RegexMatchSet@ , perform the following steps:
--
--     * Update the @RegexMatchSet@ to remove filters, if any. For more information, see 'UpdateRegexMatchSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteRegexMatchSet@ request.
--
--     * Submit a @DeleteRegexMatchSet@ request.
--
--
--
module Network.AWS.WAFRegional.DeleteRegexMatchSet
    (
    -- * Creating a Request
      deleteRegexMatchSet
    , DeleteRegexMatchSet
    -- * Request Lenses
    , drmsRegexMatchSetId
    , drmsChangeToken

    -- * Destructuring the Response
    , deleteRegexMatchSetResponse
    , DeleteRegexMatchSetResponse
    -- * Response Lenses
    , drmsrsChangeToken
    , drmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'deleteRegexMatchSet' smart constructor.
data DeleteRegexMatchSet = DeleteRegexMatchSet'
  { _drmsRegexMatchSetId :: !Text
  , _drmsChangeToken     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRegexMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drmsRegexMatchSetId' - The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to delete. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- * 'drmsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteRegexMatchSet
    :: Text -- ^ 'drmsRegexMatchSetId'
    -> Text -- ^ 'drmsChangeToken'
    -> DeleteRegexMatchSet
deleteRegexMatchSet pRegexMatchSetId_ pChangeToken_ =
  DeleteRegexMatchSet'
    {_drmsRegexMatchSetId = pRegexMatchSetId_, _drmsChangeToken = pChangeToken_}


-- | The @RegexMatchSetId@ of the 'RegexMatchSet' that you want to delete. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
drmsRegexMatchSetId :: Lens' DeleteRegexMatchSet Text
drmsRegexMatchSetId = lens _drmsRegexMatchSetId (\ s a -> s{_drmsRegexMatchSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
drmsChangeToken :: Lens' DeleteRegexMatchSet Text
drmsChangeToken = lens _drmsChangeToken (\ s a -> s{_drmsChangeToken = a})

instance AWSRequest DeleteRegexMatchSet where
        type Rs DeleteRegexMatchSet =
             DeleteRegexMatchSetResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRegexMatchSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteRegexMatchSet where

instance NFData DeleteRegexMatchSet where

instance ToHeaders DeleteRegexMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DeleteRegexMatchSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRegexMatchSet where
        toJSON DeleteRegexMatchSet'{..}
          = object
              (catMaybes
                 [Just ("RegexMatchSetId" .= _drmsRegexMatchSetId),
                  Just ("ChangeToken" .= _drmsChangeToken)])

instance ToPath DeleteRegexMatchSet where
        toPath = const "/"

instance ToQuery DeleteRegexMatchSet where
        toQuery = const mempty

-- | /See:/ 'deleteRegexMatchSetResponse' smart constructor.
data DeleteRegexMatchSetResponse = DeleteRegexMatchSetResponse'
  { _drmsrsChangeToken    :: !(Maybe Text)
  , _drmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRegexMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drmsrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'drmsrsResponseStatus' - -- | The response status code.
deleteRegexMatchSetResponse
    :: Int -- ^ 'drmsrsResponseStatus'
    -> DeleteRegexMatchSetResponse
deleteRegexMatchSetResponse pResponseStatus_ =
  DeleteRegexMatchSetResponse'
    {_drmsrsChangeToken = Nothing, _drmsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteRegexMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
drmsrsChangeToken :: Lens' DeleteRegexMatchSetResponse (Maybe Text)
drmsrsChangeToken = lens _drmsrsChangeToken (\ s a -> s{_drmsrsChangeToken = a})

-- | -- | The response status code.
drmsrsResponseStatus :: Lens' DeleteRegexMatchSetResponse Int
drmsrsResponseStatus = lens _drmsrsResponseStatus (\ s a -> s{_drmsrsResponseStatus = a})

instance NFData DeleteRegexMatchSetResponse where
