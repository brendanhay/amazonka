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
-- Module      : Network.AWS.WAFRegional.DeleteGeoMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'GeoMatchSet' . You can't delete a @GeoMatchSet@ if it's still used in any @Rules@ or if it still includes any countries.
--
--
-- If you just want to remove a @GeoMatchSet@ from a @Rule@ , use 'UpdateRule' .
--
-- To permanently delete a @GeoMatchSet@ from AWS WAF, perform the following steps:
--
--     * Update the @GeoMatchSet@ to remove any countries. For more information, see 'UpdateGeoMatchSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteGeoMatchSet@ request.
--
--     * Submit a @DeleteGeoMatchSet@ request.
--
--
--
module Network.AWS.WAFRegional.DeleteGeoMatchSet
    (
    -- * Creating a Request
      deleteGeoMatchSet
    , DeleteGeoMatchSet
    -- * Request Lenses
    , dgmsGeoMatchSetId
    , dgmsChangeToken

    -- * Destructuring the Response
    , deleteGeoMatchSetResponse
    , DeleteGeoMatchSetResponse
    -- * Response Lenses
    , dgmsrsChangeToken
    , dgmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'deleteGeoMatchSet' smart constructor.
data DeleteGeoMatchSet = DeleteGeoMatchSet'
  { _dgmsGeoMatchSetId :: !Text
  , _dgmsChangeToken   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGeoMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgmsGeoMatchSetId' - The @GeoMatchSetID@ of the 'GeoMatchSet' that you want to delete. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- * 'dgmsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteGeoMatchSet
    :: Text -- ^ 'dgmsGeoMatchSetId'
    -> Text -- ^ 'dgmsChangeToken'
    -> DeleteGeoMatchSet
deleteGeoMatchSet pGeoMatchSetId_ pChangeToken_ =
  DeleteGeoMatchSet'
    {_dgmsGeoMatchSetId = pGeoMatchSetId_, _dgmsChangeToken = pChangeToken_}


-- | The @GeoMatchSetID@ of the 'GeoMatchSet' that you want to delete. @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
dgmsGeoMatchSetId :: Lens' DeleteGeoMatchSet Text
dgmsGeoMatchSetId = lens _dgmsGeoMatchSetId (\ s a -> s{_dgmsGeoMatchSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
dgmsChangeToken :: Lens' DeleteGeoMatchSet Text
dgmsChangeToken = lens _dgmsChangeToken (\ s a -> s{_dgmsChangeToken = a})

instance AWSRequest DeleteGeoMatchSet where
        type Rs DeleteGeoMatchSet = DeleteGeoMatchSetResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 DeleteGeoMatchSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteGeoMatchSet where

instance NFData DeleteGeoMatchSet where

instance ToHeaders DeleteGeoMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DeleteGeoMatchSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteGeoMatchSet where
        toJSON DeleteGeoMatchSet'{..}
          = object
              (catMaybes
                 [Just ("GeoMatchSetId" .= _dgmsGeoMatchSetId),
                  Just ("ChangeToken" .= _dgmsChangeToken)])

instance ToPath DeleteGeoMatchSet where
        toPath = const "/"

instance ToQuery DeleteGeoMatchSet where
        toQuery = const mempty

-- | /See:/ 'deleteGeoMatchSetResponse' smart constructor.
data DeleteGeoMatchSetResponse = DeleteGeoMatchSetResponse'
  { _dgmsrsChangeToken    :: !(Maybe Text)
  , _dgmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteGeoMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgmsrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'dgmsrsResponseStatus' - -- | The response status code.
deleteGeoMatchSetResponse
    :: Int -- ^ 'dgmsrsResponseStatus'
    -> DeleteGeoMatchSetResponse
deleteGeoMatchSetResponse pResponseStatus_ =
  DeleteGeoMatchSetResponse'
    {_dgmsrsChangeToken = Nothing, _dgmsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteGeoMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
dgmsrsChangeToken :: Lens' DeleteGeoMatchSetResponse (Maybe Text)
dgmsrsChangeToken = lens _dgmsrsChangeToken (\ s a -> s{_dgmsrsChangeToken = a})

-- | -- | The response status code.
dgmsrsResponseStatus :: Lens' DeleteGeoMatchSetResponse Int
dgmsrsResponseStatus = lens _dgmsrsResponseStatus (\ s a -> s{_dgmsrsResponseStatus = a})

instance NFData DeleteGeoMatchSetResponse where
