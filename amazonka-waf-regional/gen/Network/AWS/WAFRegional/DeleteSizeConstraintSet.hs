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
-- Module      : Network.AWS.WAFRegional.DeleteSizeConstraintSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'SizeConstraintSet' . You can't delete a @SizeConstraintSet@ if it's still used in any @Rules@ or if it still includes any 'SizeConstraint' objects (any filters).
--
--
-- If you just want to remove a @SizeConstraintSet@ from a @Rule@ , use 'UpdateRule' .
--
-- To permanently delete a @SizeConstraintSet@ , perform the following steps:
--
--     * Update the @SizeConstraintSet@ to remove filters, if any. For more information, see 'UpdateSizeConstraintSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteSizeConstraintSet@ request.
--
--     * Submit a @DeleteSizeConstraintSet@ request.
--
--
--
module Network.AWS.WAFRegional.DeleteSizeConstraintSet
    (
    -- * Creating a Request
      deleteSizeConstraintSet
    , DeleteSizeConstraintSet
    -- * Request Lenses
    , dscsSizeConstraintSetId
    , dscsChangeToken

    -- * Destructuring the Response
    , deleteSizeConstraintSetResponse
    , DeleteSizeConstraintSetResponse
    -- * Response Lenses
    , dscsrsChangeToken
    , dscsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'deleteSizeConstraintSet' smart constructor.
data DeleteSizeConstraintSet = DeleteSizeConstraintSet'
  { _dscsSizeConstraintSetId :: !Text
  , _dscsChangeToken         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSizeConstraintSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscsSizeConstraintSetId' - The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to delete. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
--
-- * 'dscsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteSizeConstraintSet
    :: Text -- ^ 'dscsSizeConstraintSetId'
    -> Text -- ^ 'dscsChangeToken'
    -> DeleteSizeConstraintSet
deleteSizeConstraintSet pSizeConstraintSetId_ pChangeToken_ =
  DeleteSizeConstraintSet'
    { _dscsSizeConstraintSetId = pSizeConstraintSetId_
    , _dscsChangeToken = pChangeToken_
    }


-- | The @SizeConstraintSetId@ of the 'SizeConstraintSet' that you want to delete. @SizeConstraintSetId@ is returned by 'CreateSizeConstraintSet' and by 'ListSizeConstraintSets' .
dscsSizeConstraintSetId :: Lens' DeleteSizeConstraintSet Text
dscsSizeConstraintSetId = lens _dscsSizeConstraintSetId (\ s a -> s{_dscsSizeConstraintSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
dscsChangeToken :: Lens' DeleteSizeConstraintSet Text
dscsChangeToken = lens _dscsChangeToken (\ s a -> s{_dscsChangeToken = a})

instance AWSRequest DeleteSizeConstraintSet where
        type Rs DeleteSizeConstraintSet =
             DeleteSizeConstraintSetResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSizeConstraintSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteSizeConstraintSet where

instance NFData DeleteSizeConstraintSet where

instance ToHeaders DeleteSizeConstraintSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DeleteSizeConstraintSet"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSizeConstraintSet where
        toJSON DeleteSizeConstraintSet'{..}
          = object
              (catMaybes
                 [Just
                    ("SizeConstraintSetId" .= _dscsSizeConstraintSetId),
                  Just ("ChangeToken" .= _dscsChangeToken)])

instance ToPath DeleteSizeConstraintSet where
        toPath = const "/"

instance ToQuery DeleteSizeConstraintSet where
        toQuery = const mempty

-- | /See:/ 'deleteSizeConstraintSetResponse' smart constructor.
data DeleteSizeConstraintSetResponse = DeleteSizeConstraintSetResponse'
  { _dscsrsChangeToken    :: !(Maybe Text)
  , _dscsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSizeConstraintSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscsrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'dscsrsResponseStatus' - -- | The response status code.
deleteSizeConstraintSetResponse
    :: Int -- ^ 'dscsrsResponseStatus'
    -> DeleteSizeConstraintSetResponse
deleteSizeConstraintSetResponse pResponseStatus_ =
  DeleteSizeConstraintSetResponse'
    {_dscsrsChangeToken = Nothing, _dscsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteSizeConstraintSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
dscsrsChangeToken :: Lens' DeleteSizeConstraintSetResponse (Maybe Text)
dscsrsChangeToken = lens _dscsrsChangeToken (\ s a -> s{_dscsrsChangeToken = a})

-- | -- | The response status code.
dscsrsResponseStatus :: Lens' DeleteSizeConstraintSetResponse Int
dscsrsResponseStatus = lens _dscsrsResponseStatus (\ s a -> s{_dscsrsResponseStatus = a})

instance NFData DeleteSizeConstraintSetResponse where
