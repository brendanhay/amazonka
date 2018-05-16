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
-- Module      : Network.AWS.WAFRegional.DeleteIPSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an 'IPSet' . You can't delete an @IPSet@ if it's still used in any @Rules@ or if it still includes any IP addresses.
--
--
-- If you just want to remove an @IPSet@ from a @Rule@ , use 'UpdateRule' .
--
-- To permanently delete an @IPSet@ from AWS WAF, perform the following steps:
--
--     * Update the @IPSet@ to remove IP address ranges, if any. For more information, see 'UpdateIPSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteIPSet@ request.
--
--     * Submit a @DeleteIPSet@ request.
--
--
--
module Network.AWS.WAFRegional.DeleteIPSet
    (
    -- * Creating a Request
      deleteIPSet
    , DeleteIPSet
    -- * Request Lenses
    , disIPSetId
    , disChangeToken

    -- * Destructuring the Response
    , deleteIPSetResponse
    , DeleteIPSetResponse
    -- * Response Lenses
    , disrsChangeToken
    , disrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'deleteIPSet' smart constructor.
data DeleteIPSet = DeleteIPSet'
  { _disIPSetId     :: !Text
  , _disChangeToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIPSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disIPSetId' - The @IPSetId@ of the 'IPSet' that you want to delete. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
--
-- * 'disChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteIPSet
    :: Text -- ^ 'disIPSetId'
    -> Text -- ^ 'disChangeToken'
    -> DeleteIPSet
deleteIPSet pIPSetId_ pChangeToken_ =
  DeleteIPSet' {_disIPSetId = pIPSetId_, _disChangeToken = pChangeToken_}


-- | The @IPSetId@ of the 'IPSet' that you want to delete. @IPSetId@ is returned by 'CreateIPSet' and by 'ListIPSets' .
disIPSetId :: Lens' DeleteIPSet Text
disIPSetId = lens _disIPSetId (\ s a -> s{_disIPSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
disChangeToken :: Lens' DeleteIPSet Text
disChangeToken = lens _disChangeToken (\ s a -> s{_disChangeToken = a})

instance AWSRequest DeleteIPSet where
        type Rs DeleteIPSet = DeleteIPSetResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 DeleteIPSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteIPSet where

instance NFData DeleteIPSet where

instance ToHeaders DeleteIPSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DeleteIPSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteIPSet where
        toJSON DeleteIPSet'{..}
          = object
              (catMaybes
                 [Just ("IPSetId" .= _disIPSetId),
                  Just ("ChangeToken" .= _disChangeToken)])

instance ToPath DeleteIPSet where
        toPath = const "/"

instance ToQuery DeleteIPSet where
        toQuery = const mempty

-- | /See:/ 'deleteIPSetResponse' smart constructor.
data DeleteIPSetResponse = DeleteIPSetResponse'
  { _disrsChangeToken    :: !(Maybe Text)
  , _disrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteIPSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'disrsResponseStatus' - -- | The response status code.
deleteIPSetResponse
    :: Int -- ^ 'disrsResponseStatus'
    -> DeleteIPSetResponse
deleteIPSetResponse pResponseStatus_ =
  DeleteIPSetResponse'
    {_disrsChangeToken = Nothing, _disrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteIPSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
disrsChangeToken :: Lens' DeleteIPSetResponse (Maybe Text)
disrsChangeToken = lens _disrsChangeToken (\ s a -> s{_disrsChangeToken = a})

-- | -- | The response status code.
disrsResponseStatus :: Lens' DeleteIPSetResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\ s a -> s{_disrsResponseStatus = a})

instance NFData DeleteIPSetResponse where
