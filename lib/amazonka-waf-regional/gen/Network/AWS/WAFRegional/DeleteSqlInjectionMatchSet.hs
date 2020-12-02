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
-- Module      : Network.AWS.WAFRegional.DeleteSqlInjectionMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'SqlInjectionMatchSet' . You can't delete a @SqlInjectionMatchSet@ if it's still used in any @Rules@ or if it still contains any 'SqlInjectionMatchTuple' objects.
--
--
-- If you just want to remove a @SqlInjectionMatchSet@ from a @Rule@ , use 'UpdateRule' .
--
-- To permanently delete a @SqlInjectionMatchSet@ from AWS WAF, perform the following steps:
--
--     * Update the @SqlInjectionMatchSet@ to remove filters, if any. For more information, see 'UpdateSqlInjectionMatchSet' .
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteSqlInjectionMatchSet@ request.
--
--     * Submit a @DeleteSqlInjectionMatchSet@ request.
--
--
--
module Network.AWS.WAFRegional.DeleteSqlInjectionMatchSet
    (
    -- * Creating a Request
      deleteSqlInjectionMatchSet
    , DeleteSqlInjectionMatchSet
    -- * Request Lenses
    , dsimsSqlInjectionMatchSetId
    , dsimsChangeToken

    -- * Destructuring the Response
    , deleteSqlInjectionMatchSetResponse
    , DeleteSqlInjectionMatchSetResponse
    -- * Response Lenses
    , dsimsrsChangeToken
    , dsimsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | A request to delete a 'SqlInjectionMatchSet' from AWS WAF.
--
--
--
-- /See:/ 'deleteSqlInjectionMatchSet' smart constructor.
data DeleteSqlInjectionMatchSet = DeleteSqlInjectionMatchSet'
  { _dsimsSqlInjectionMatchSetId :: !Text
  , _dsimsChangeToken            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSqlInjectionMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsimsSqlInjectionMatchSetId' - The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to delete. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- * 'dsimsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
deleteSqlInjectionMatchSet
    :: Text -- ^ 'dsimsSqlInjectionMatchSetId'
    -> Text -- ^ 'dsimsChangeToken'
    -> DeleteSqlInjectionMatchSet
deleteSqlInjectionMatchSet pSqlInjectionMatchSetId_ pChangeToken_ =
  DeleteSqlInjectionMatchSet'
    { _dsimsSqlInjectionMatchSetId = pSqlInjectionMatchSetId_
    , _dsimsChangeToken = pChangeToken_
    }


-- | The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to delete. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
dsimsSqlInjectionMatchSetId :: Lens' DeleteSqlInjectionMatchSet Text
dsimsSqlInjectionMatchSetId = lens _dsimsSqlInjectionMatchSetId (\ s a -> s{_dsimsSqlInjectionMatchSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
dsimsChangeToken :: Lens' DeleteSqlInjectionMatchSet Text
dsimsChangeToken = lens _dsimsChangeToken (\ s a -> s{_dsimsChangeToken = a})

instance AWSRequest DeleteSqlInjectionMatchSet where
        type Rs DeleteSqlInjectionMatchSet =
             DeleteSqlInjectionMatchSetResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSqlInjectionMatchSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable DeleteSqlInjectionMatchSet where

instance NFData DeleteSqlInjectionMatchSet where

instance ToHeaders DeleteSqlInjectionMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.DeleteSqlInjectionMatchSet"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSqlInjectionMatchSet where
        toJSON DeleteSqlInjectionMatchSet'{..}
          = object
              (catMaybes
                 [Just
                    ("SqlInjectionMatchSetId" .=
                       _dsimsSqlInjectionMatchSetId),
                  Just ("ChangeToken" .= _dsimsChangeToken)])

instance ToPath DeleteSqlInjectionMatchSet where
        toPath = const "/"

instance ToQuery DeleteSqlInjectionMatchSet where
        toQuery = const mempty

-- | The response to a request to delete a 'SqlInjectionMatchSet' from AWS WAF.
--
--
--
-- /See:/ 'deleteSqlInjectionMatchSetResponse' smart constructor.
data DeleteSqlInjectionMatchSetResponse = DeleteSqlInjectionMatchSetResponse'
  { _dsimsrsChangeToken    :: !(Maybe Text)
  , _dsimsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSqlInjectionMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsimsrsChangeToken' - The @ChangeToken@ that you used to submit the @DeleteSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'dsimsrsResponseStatus' - -- | The response status code.
deleteSqlInjectionMatchSetResponse
    :: Int -- ^ 'dsimsrsResponseStatus'
    -> DeleteSqlInjectionMatchSetResponse
deleteSqlInjectionMatchSetResponse pResponseStatus_ =
  DeleteSqlInjectionMatchSetResponse'
    {_dsimsrsChangeToken = Nothing, _dsimsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @DeleteSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
dsimsrsChangeToken :: Lens' DeleteSqlInjectionMatchSetResponse (Maybe Text)
dsimsrsChangeToken = lens _dsimsrsChangeToken (\ s a -> s{_dsimsrsChangeToken = a})

-- | -- | The response status code.
dsimsrsResponseStatus :: Lens' DeleteSqlInjectionMatchSetResponse Int
dsimsrsResponseStatus = lens _dsimsrsResponseStatus (\ s a -> s{_dsimsrsResponseStatus = a})

instance NFData DeleteSqlInjectionMatchSetResponse
         where
