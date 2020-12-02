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
-- Module      : Network.AWS.WAFRegional.DeleteByteMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'ByteMatchSet' . You can't delete a @ByteMatchSet@ if it's still used in any @Rules@ or if it still includes any 'ByteMatchTuple' objects (any filters).
--
--
-- If you just want to remove a @ByteMatchSet@ from a @Rule@ , use 'UpdateRule' .
--
-- To permanently delete a @ByteMatchSet@ , perform the following steps:
--
--     * Update the @ByteMatchSet@ to remove filters, if any. For more information, see 'UpdateByteMatchSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteByteMatchSet@ request.
--
--     * Submit a @DeleteByteMatchSet@ request.
--
--
--
module Network.AWS.WAFRegional.DeleteByteMatchSet
    (
    -- * Creating a Request
      deleteByteMatchSet
    , DeleteByteMatchSet
    -- * Request Lenses
    , dbmsByteMatchSetId
    , dbmsChangeToken

    -- * Destructuring the Response
    , deleteByteMatchSetResponse
    , DeleteByteMatchSetResponse
    -- * Response Lenses
    , dbmsrsChangeToken
    , dbmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'deleteByteMatchSet' smart constructor.
data DeleteByteMatchSet = DeleteByteMatchSet'
  { _dbmsByteMatchSetId :: !Text
  , _dbmsChangeToken    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteByteMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbmsByteMatchSetId' - The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to delete. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- * 'dbmsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteByteMatchSet
    :: Text -- ^ 'dbmsByteMatchSetId'
    -> Text -- ^ 'dbmsChangeToken'
    -> DeleteByteMatchSet
deleteByteMatchSet pByteMatchSetId_ pChangeToken_ =
  DeleteByteMatchSet'
    {_dbmsByteMatchSetId = pByteMatchSetId_, _dbmsChangeToken = pChangeToken_}


-- | The @ByteMatchSetId@ of the 'ByteMatchSet' that you want to delete. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
dbmsByteMatchSetId :: Lens' DeleteByteMatchSet Text
dbmsByteMatchSetId = lens _dbmsByteMatchSetId (\ s a -> s{_dbmsByteMatchSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
dbmsChangeToken :: Lens' DeleteByteMatchSet Text
dbmsChangeToken = lens _dbmsChangeToken (\ s a -> s{_dbmsChangeToken = a})

instance AWSRequest DeleteByteMatchSet where
        type Rs DeleteByteMatchSet =
             DeleteByteMatchSetResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 DeleteByteMatchSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteByteMatchSet where

instance NFData DeleteByteMatchSet where

instance ToHeaders DeleteByteMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DeleteByteMatchSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteByteMatchSet where
        toJSON DeleteByteMatchSet'{..}
          = object
              (catMaybes
                 [Just ("ByteMatchSetId" .= _dbmsByteMatchSetId),
                  Just ("ChangeToken" .= _dbmsChangeToken)])

instance ToPath DeleteByteMatchSet where
        toPath = const "/"

instance ToQuery DeleteByteMatchSet where
        toQuery = const mempty

-- | /See:/ 'deleteByteMatchSetResponse' smart constructor.
data DeleteByteMatchSetResponse = DeleteByteMatchSetResponse'
  { _dbmsrsChangeToken    :: !(Maybe Text)
  , _dbmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteByteMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbmsrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'dbmsrsResponseStatus' - -- | The response status code.
deleteByteMatchSetResponse
    :: Int -- ^ 'dbmsrsResponseStatus'
    -> DeleteByteMatchSetResponse
deleteByteMatchSetResponse pResponseStatus_ =
  DeleteByteMatchSetResponse'
    {_dbmsrsChangeToken = Nothing, _dbmsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteByteMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
dbmsrsChangeToken :: Lens' DeleteByteMatchSetResponse (Maybe Text)
dbmsrsChangeToken = lens _dbmsrsChangeToken (\ s a -> s{_dbmsrsChangeToken = a})

-- | -- | The response status code.
dbmsrsResponseStatus :: Lens' DeleteByteMatchSetResponse Int
dbmsrsResponseStatus = lens _dbmsrsResponseStatus (\ s a -> s{_dbmsrsResponseStatus = a})

instance NFData DeleteByteMatchSetResponse where
