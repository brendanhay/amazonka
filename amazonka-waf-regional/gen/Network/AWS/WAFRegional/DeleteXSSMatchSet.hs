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
-- Module      : Network.AWS.WAFRegional.DeleteXSSMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an 'XssMatchSet' . You can't delete an @XssMatchSet@ if it's still used in any @Rules@ or if it still contains any 'XssMatchTuple' objects.
--
--
-- If you just want to remove an @XssMatchSet@ from a @Rule@ , use 'UpdateRule' .
--
-- To permanently delete an @XssMatchSet@ from AWS WAF, perform the following steps:
--
--     * Update the @XssMatchSet@ to remove filters, if any. For more information, see 'UpdateXssMatchSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteXssMatchSet@ request.
--
--     * Submit a @DeleteXssMatchSet@ request.
--
--
--
module Network.AWS.WAFRegional.DeleteXSSMatchSet
    (
    -- * Creating a Request
      deleteXSSMatchSet
    , DeleteXSSMatchSet
    -- * Request Lenses
    , dxmsXSSMatchSetId
    , dxmsChangeToken

    -- * Destructuring the Response
    , deleteXSSMatchSetResponse
    , DeleteXSSMatchSetResponse
    -- * Response Lenses
    , dxmsrsChangeToken
    , dxmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | A request to delete an 'XssMatchSet' from AWS WAF.
--
--
--
-- /See:/ 'deleteXSSMatchSet' smart constructor.
data DeleteXSSMatchSet = DeleteXSSMatchSet'
  { _dxmsXSSMatchSetId :: !Text
  , _dxmsChangeToken   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteXSSMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dxmsXSSMatchSetId' - The @XssMatchSetId@ of the 'XssMatchSet' that you want to delete. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- * 'dxmsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteXSSMatchSet
    :: Text -- ^ 'dxmsXSSMatchSetId'
    -> Text -- ^ 'dxmsChangeToken'
    -> DeleteXSSMatchSet
deleteXSSMatchSet pXSSMatchSetId_ pChangeToken_ =
  DeleteXSSMatchSet'
    {_dxmsXSSMatchSetId = pXSSMatchSetId_, _dxmsChangeToken = pChangeToken_}


-- | The @XssMatchSetId@ of the 'XssMatchSet' that you want to delete. @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
dxmsXSSMatchSetId :: Lens' DeleteXSSMatchSet Text
dxmsXSSMatchSetId = lens _dxmsXSSMatchSetId (\ s a -> s{_dxmsXSSMatchSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
dxmsChangeToken :: Lens' DeleteXSSMatchSet Text
dxmsChangeToken = lens _dxmsChangeToken (\ s a -> s{_dxmsChangeToken = a})

instance AWSRequest DeleteXSSMatchSet where
        type Rs DeleteXSSMatchSet = DeleteXSSMatchSetResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 DeleteXSSMatchSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteXSSMatchSet where

instance NFData DeleteXSSMatchSet where

instance ToHeaders DeleteXSSMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DeleteXssMatchSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteXSSMatchSet where
        toJSON DeleteXSSMatchSet'{..}
          = object
              (catMaybes
                 [Just ("XssMatchSetId" .= _dxmsXSSMatchSetId),
                  Just ("ChangeToken" .= _dxmsChangeToken)])

instance ToPath DeleteXSSMatchSet where
        toPath = const "/"

instance ToQuery DeleteXSSMatchSet where
        toQuery = const mempty

-- | The response to a request to delete an 'XssMatchSet' from AWS WAF.
--
--
--
-- /See:/ 'deleteXSSMatchSetResponse' smart constructor.
data DeleteXSSMatchSetResponse = DeleteXSSMatchSetResponse'
  { _dxmsrsChangeToken    :: !(Maybe Text)
  , _dxmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteXSSMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dxmsrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'dxmsrsResponseStatus' - -- | The response status code.
deleteXSSMatchSetResponse
    :: Int -- ^ 'dxmsrsResponseStatus'
    -> DeleteXSSMatchSetResponse
deleteXSSMatchSetResponse pResponseStatus_ =
  DeleteXSSMatchSetResponse'
    {_dxmsrsChangeToken = Nothing, _dxmsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteXssMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
dxmsrsChangeToken :: Lens' DeleteXSSMatchSetResponse (Maybe Text)
dxmsrsChangeToken = lens _dxmsrsChangeToken (\ s a -> s{_dxmsrsChangeToken = a})

-- | -- | The response status code.
dxmsrsResponseStatus :: Lens' DeleteXSSMatchSetResponse Int
dxmsrsResponseStatus = lens _dxmsrsResponseStatus (\ s a -> s{_dxmsrsResponseStatus = a})

instance NFData DeleteXSSMatchSetResponse where
