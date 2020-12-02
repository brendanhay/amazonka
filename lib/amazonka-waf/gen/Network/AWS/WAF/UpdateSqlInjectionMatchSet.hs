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
-- Module      : Network.AWS.WAF.UpdateSqlInjectionMatchSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes 'SqlInjectionMatchTuple' objects (filters) in a 'SqlInjectionMatchSet' . For each @SqlInjectionMatchTuple@ object, you specify the following values:
--
--
--     * @Action@ : Whether to insert the object into or delete the object from the array. To change a @SqlInjectionMatchTuple@ , you delete the existing object and add a new one.
--
--     * @FieldToMatch@ : The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header, the name of the header.
--
--     * @TextTransformation@ : Which text transformation, if any, to perform on the web request before inspecting the request for snippets of malicious SQL code.
--
--
--
-- You use @SqlInjectionMatchSet@ objects to specify which CloudFront requests you want to allow, block, or count. For example, if you're receiving requests that contain snippets of SQL code in the query string and you want to block the requests, you can create a @SqlInjectionMatchSet@ with the applicable settings, and then configure AWS WAF to block the requests.
--
-- To create and configure a @SqlInjectionMatchSet@ , perform the following steps:
--
--     * Submit a 'CreateSqlInjectionMatchSet' request.
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of an 'UpdateIPSet' request.
--
--     * Submit an @UpdateSqlInjectionMatchSet@ request to specify the parts of web requests that you want AWS WAF to inspect for snippets of SQL code.
--
--
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide> .
--
module Network.AWS.WAF.UpdateSqlInjectionMatchSet
    (
    -- * Creating a Request
      updateSqlInjectionMatchSet
    , UpdateSqlInjectionMatchSet
    -- * Request Lenses
    , usimsSqlInjectionMatchSetId
    , usimsChangeToken
    , usimsUpdates

    -- * Destructuring the Response
    , updateSqlInjectionMatchSetResponse
    , UpdateSqlInjectionMatchSetResponse
    -- * Response Lenses
    , usimsrsChangeToken
    , usimsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | A request to update a 'SqlInjectionMatchSet' .
--
--
--
-- /See:/ 'updateSqlInjectionMatchSet' smart constructor.
data UpdateSqlInjectionMatchSet = UpdateSqlInjectionMatchSet'
  { _usimsSqlInjectionMatchSetId :: !Text
  , _usimsChangeToken            :: !Text
  , _usimsUpdates                :: !(List1 SqlInjectionMatchSetUpdate)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSqlInjectionMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usimsSqlInjectionMatchSetId' - The @SqlInjectionMatchSetId@ of the @SqlInjectionMatchSet@ that you want to update. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- * 'usimsChangeToken' - The value returned by the most recent call to 'GetChangeToken' .
--
-- * 'usimsUpdates' - An array of @SqlInjectionMatchSetUpdate@ objects that you want to insert into or delete from a 'SqlInjectionMatchSet' . For more information, see the applicable data types:     * 'SqlInjectionMatchSetUpdate' : Contains @Action@ and @SqlInjectionMatchTuple@      * 'SqlInjectionMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@      * 'FieldToMatch' : Contains @Data@ and @Type@
updateSqlInjectionMatchSet
    :: Text -- ^ 'usimsSqlInjectionMatchSetId'
    -> Text -- ^ 'usimsChangeToken'
    -> NonEmpty SqlInjectionMatchSetUpdate -- ^ 'usimsUpdates'
    -> UpdateSqlInjectionMatchSet
updateSqlInjectionMatchSet pSqlInjectionMatchSetId_ pChangeToken_ pUpdates_ =
  UpdateSqlInjectionMatchSet'
    { _usimsSqlInjectionMatchSetId = pSqlInjectionMatchSetId_
    , _usimsChangeToken = pChangeToken_
    , _usimsUpdates = _List1 # pUpdates_
    }


-- | The @SqlInjectionMatchSetId@ of the @SqlInjectionMatchSet@ that you want to update. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
usimsSqlInjectionMatchSetId :: Lens' UpdateSqlInjectionMatchSet Text
usimsSqlInjectionMatchSetId = lens _usimsSqlInjectionMatchSetId (\ s a -> s{_usimsSqlInjectionMatchSetId = a})

-- | The value returned by the most recent call to 'GetChangeToken' .
usimsChangeToken :: Lens' UpdateSqlInjectionMatchSet Text
usimsChangeToken = lens _usimsChangeToken (\ s a -> s{_usimsChangeToken = a})

-- | An array of @SqlInjectionMatchSetUpdate@ objects that you want to insert into or delete from a 'SqlInjectionMatchSet' . For more information, see the applicable data types:     * 'SqlInjectionMatchSetUpdate' : Contains @Action@ and @SqlInjectionMatchTuple@      * 'SqlInjectionMatchTuple' : Contains @FieldToMatch@ and @TextTransformation@      * 'FieldToMatch' : Contains @Data@ and @Type@
usimsUpdates :: Lens' UpdateSqlInjectionMatchSet (NonEmpty SqlInjectionMatchSetUpdate)
usimsUpdates = lens _usimsUpdates (\ s a -> s{_usimsUpdates = a}) . _List1

instance AWSRequest UpdateSqlInjectionMatchSet where
        type Rs UpdateSqlInjectionMatchSet =
             UpdateSqlInjectionMatchSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSqlInjectionMatchSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateSqlInjectionMatchSet where

instance NFData UpdateSqlInjectionMatchSet where

instance ToHeaders UpdateSqlInjectionMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.UpdateSqlInjectionMatchSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSqlInjectionMatchSet where
        toJSON UpdateSqlInjectionMatchSet'{..}
          = object
              (catMaybes
                 [Just
                    ("SqlInjectionMatchSetId" .=
                       _usimsSqlInjectionMatchSetId),
                  Just ("ChangeToken" .= _usimsChangeToken),
                  Just ("Updates" .= _usimsUpdates)])

instance ToPath UpdateSqlInjectionMatchSet where
        toPath = const "/"

instance ToQuery UpdateSqlInjectionMatchSet where
        toQuery = const mempty

-- | The response to an 'UpdateSqlInjectionMatchSets' request.
--
--
--
-- /See:/ 'updateSqlInjectionMatchSetResponse' smart constructor.
data UpdateSqlInjectionMatchSetResponse = UpdateSqlInjectionMatchSetResponse'
  { _usimsrsChangeToken    :: !(Maybe Text)
  , _usimsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSqlInjectionMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usimsrsChangeToken' - The @ChangeToken@ that you used to submit the @UpdateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- * 'usimsrsResponseStatus' - -- | The response status code.
updateSqlInjectionMatchSetResponse
    :: Int -- ^ 'usimsrsResponseStatus'
    -> UpdateSqlInjectionMatchSetResponse
updateSqlInjectionMatchSetResponse pResponseStatus_ =
  UpdateSqlInjectionMatchSetResponse'
    {_usimsrsChangeToken = Nothing, _usimsrsResponseStatus = pResponseStatus_}


-- | The @ChangeToken@ that you used to submit the @UpdateSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
usimsrsChangeToken :: Lens' UpdateSqlInjectionMatchSetResponse (Maybe Text)
usimsrsChangeToken = lens _usimsrsChangeToken (\ s a -> s{_usimsrsChangeToken = a})

-- | -- | The response status code.
usimsrsResponseStatus :: Lens' UpdateSqlInjectionMatchSetResponse Int
usimsrsResponseStatus = lens _usimsrsResponseStatus (\ s a -> s{_usimsrsResponseStatus = a})

instance NFData UpdateSqlInjectionMatchSetResponse
         where
