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
-- Module      : Network.AWS.WAF.DeleteRegexPatternSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'RegexPatternSet' . You can't delete a @RegexPatternSet@ if it's still used in any @RegexMatchSet@ or if the @RegexPatternSet@ is not empty.
--
--
module Network.AWS.WAF.DeleteRegexPatternSet
    (
    -- * Creating a Request
      deleteRegexPatternSet
    , DeleteRegexPatternSet
    -- * Request Lenses
    , drpsRegexPatternSetId
    , drpsChangeToken

    -- * Destructuring the Response
    , deleteRegexPatternSetResponse
    , DeleteRegexPatternSetResponse
    -- * Response Lenses
    , drpsrsChangeToken
    , drpsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'deleteRegexPatternSet' smart constructor.
data DeleteRegexPatternSet = DeleteRegexPatternSet'
  { _drpsRegexPatternSetId :: !Text
  , _drpsChangeToken       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRegexPatternSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpsRegexPatternSetId' - The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to delete. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- * 'drpsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteRegexPatternSet
    :: Text -- ^ 'drpsRegexPatternSetId'
    -> Text -- ^ 'drpsChangeToken'
    -> DeleteRegexPatternSet
deleteRegexPatternSet pRegexPatternSetId_ pChangeToken_ =
  DeleteRegexPatternSet'
    { _drpsRegexPatternSetId = pRegexPatternSetId_
    , _drpsChangeToken = pChangeToken_
    }


-- | The @RegexPatternSetId@ of the 'RegexPatternSet' that you want to delete. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
drpsRegexPatternSetId :: Lens' DeleteRegexPatternSet Text
drpsRegexPatternSetId = lens _drpsRegexPatternSetId (\ s a -> s{_drpsRegexPatternSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
drpsChangeToken :: Lens' DeleteRegexPatternSet Text
drpsChangeToken = lens _drpsChangeToken (\ s a -> s{_drpsChangeToken = a})

instance AWSRequest DeleteRegexPatternSet where
        type Rs DeleteRegexPatternSet =
             DeleteRegexPatternSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 DeleteRegexPatternSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteRegexPatternSet where

instance NFData DeleteRegexPatternSet where

instance ToHeaders DeleteRegexPatternSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.DeleteRegexPatternSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRegexPatternSet where
        toJSON DeleteRegexPatternSet'{..}
          = object
              (catMaybes
                 [Just
                    ("RegexPatternSetId" .= _drpsRegexPatternSetId),
                  Just ("ChangeToken" .= _drpsChangeToken)])

instance ToPath DeleteRegexPatternSet where
        toPath = const "/"

instance ToQuery DeleteRegexPatternSet where
        toQuery = const mempty

-- | /See:/ 'deleteRegexPatternSetResponse' smart constructor.
data DeleteRegexPatternSetResponse = DeleteRegexPatternSetResponse'
  { _drpsrsChangeToken    :: !(Maybe Text)
  , _drpsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRegexPatternSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpsrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'drpsrsResponseStatus' - -- | The response status code.
deleteRegexPatternSetResponse
    :: Int -- ^ 'drpsrsResponseStatus'
    -> DeleteRegexPatternSetResponse
deleteRegexPatternSetResponse pResponseStatus_ =
  DeleteRegexPatternSetResponse'
    {_drpsrsChangeToken = Nothing, _drpsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteRegexPatternSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
drpsrsChangeToken :: Lens' DeleteRegexPatternSetResponse (Maybe Text)
drpsrsChangeToken = lens _drpsrsChangeToken (\ s a -> s{_drpsrsChangeToken = a})

-- | -- | The response status code.
drpsrsResponseStatus :: Lens' DeleteRegexPatternSetResponse Int
drpsrsResponseStatus = lens _drpsrsResponseStatus (\ s a -> s{_drpsrsResponseStatus = a})

instance NFData DeleteRegexPatternSetResponse where
