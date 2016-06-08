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
-- Module      : Network.AWS.WAF.UpdateXSSMatchSet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes < XssMatchTuple> objects (filters) in an < XssMatchSet>. For each 'XssMatchTuple' object, you specify the following values:
--
-- -   'Action': Whether to insert the object into or delete the object from the array. To change a 'XssMatchTuple', you delete the existing object and add a new one.
-- -   'FieldToMatch': The part of web requests that you want AWS WAF to inspect and, if you want AWS WAF to inspect a header, the name of the header.
-- -   'TextTransformation': Which text transformation, if any, to perform on the web request before inspecting the request for cross-site scripting attacks.
--
-- You use 'XssMatchSet' objects to specify which CloudFront requests you want to allow, block, or count. For example, if you\'re receiving requests that contain cross-site scripting attacks in the request body and you want to block the requests, you can create an 'XssMatchSet' with the applicable settings, and then configure AWS WAF to block the requests.
--
-- To create and configure an 'XssMatchSet', perform the following steps:
--
-- 1.  Submit a < CreateXssMatchSet> request.
-- 2.  Use < GetChangeToken> to get the change token that you provide in the 'ChangeToken' parameter of an < UpdateIPSet> request.
-- 3.  Submit an 'UpdateXssMatchSet' request to specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks.
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAF.UpdateXSSMatchSet
    (
    -- * Creating a Request
      updateXSSMatchSet
    , UpdateXSSMatchSet
    -- * Request Lenses
    , uxmsXSSMatchSetId
    , uxmsChangeToken
    , uxmsUpdates

    -- * Destructuring the Response
    , updateXSSMatchSetResponse
    , UpdateXSSMatchSetResponse
    -- * Response Lenses
    , uxmsrsChangeToken
    , uxmsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WAF.Types
import           Network.AWS.WAF.Types.Product

-- | A request to update an < XssMatchSet>.
--
-- /See:/ 'updateXSSMatchSet' smart constructor.
data UpdateXSSMatchSet = UpdateXSSMatchSet'
    { _uxmsXSSMatchSetId :: !Text
    , _uxmsChangeToken   :: !Text
    , _uxmsUpdates       :: ![XSSMatchSetUpdate]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateXSSMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uxmsXSSMatchSetId'
--
-- * 'uxmsChangeToken'
--
-- * 'uxmsUpdates'
updateXSSMatchSet
    :: Text -- ^ 'uxmsXSSMatchSetId'
    -> Text -- ^ 'uxmsChangeToken'
    -> UpdateXSSMatchSet
updateXSSMatchSet pXSSMatchSetId_ pChangeToken_ =
    UpdateXSSMatchSet'
    { _uxmsXSSMatchSetId = pXSSMatchSetId_
    , _uxmsChangeToken = pChangeToken_
    , _uxmsUpdates = mempty
    }

-- | The 'XssMatchSetId' of the 'XssMatchSet' that you want to update. 'XssMatchSetId' is returned by < CreateXssMatchSet> and by < ListXssMatchSets>.
uxmsXSSMatchSetId :: Lens' UpdateXSSMatchSet Text
uxmsXSSMatchSetId = lens _uxmsXSSMatchSetId (\ s a -> s{_uxmsXSSMatchSetId = a});

-- | The value returned by the most recent call to < GetChangeToken>.
uxmsChangeToken :: Lens' UpdateXSSMatchSet Text
uxmsChangeToken = lens _uxmsChangeToken (\ s a -> s{_uxmsChangeToken = a});

-- | An array of 'XssMatchSetUpdate' objects that you want to insert into or delete from a < XssMatchSet>. For more information, see the applicable data types:
--
-- -   < XssMatchSetUpdate>: Contains 'Action' and 'XssMatchTuple'
-- -   < XssMatchTuple>: Contains 'FieldToMatch' and 'TextTransformation'
-- -   < FieldToMatch>: Contains 'Data' and 'Type'
uxmsUpdates :: Lens' UpdateXSSMatchSet [XSSMatchSetUpdate]
uxmsUpdates = lens _uxmsUpdates (\ s a -> s{_uxmsUpdates = a}) . _Coerce;

instance AWSRequest UpdateXSSMatchSet where
        type Rs UpdateXSSMatchSet = UpdateXSSMatchSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 UpdateXSSMatchSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateXSSMatchSet

instance NFData UpdateXSSMatchSet

instance ToHeaders UpdateXSSMatchSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.UpdateXssMatchSet" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateXSSMatchSet where
        toJSON UpdateXSSMatchSet'{..}
          = object
              (catMaybes
                 [Just ("XssMatchSetId" .= _uxmsXSSMatchSetId),
                  Just ("ChangeToken" .= _uxmsChangeToken),
                  Just ("Updates" .= _uxmsUpdates)])

instance ToPath UpdateXSSMatchSet where
        toPath = const "/"

instance ToQuery UpdateXSSMatchSet where
        toQuery = const mempty

-- | The response to an < UpdateXssMatchSets> request.
--
-- /See:/ 'updateXSSMatchSetResponse' smart constructor.
data UpdateXSSMatchSetResponse = UpdateXSSMatchSetResponse'
    { _uxmsrsChangeToken    :: !(Maybe Text)
    , _uxmsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateXSSMatchSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uxmsrsChangeToken'
--
-- * 'uxmsrsResponseStatus'
updateXSSMatchSetResponse
    :: Int -- ^ 'uxmsrsResponseStatus'
    -> UpdateXSSMatchSetResponse
updateXSSMatchSetResponse pResponseStatus_ =
    UpdateXSSMatchSetResponse'
    { _uxmsrsChangeToken = Nothing
    , _uxmsrsResponseStatus = pResponseStatus_
    }

-- | The 'ChangeToken' that you used to submit the 'UpdateXssMatchSet' request. You can also use this value to query the status of the request. For more information, see < GetChangeTokenStatus>.
uxmsrsChangeToken :: Lens' UpdateXSSMatchSetResponse (Maybe Text)
uxmsrsChangeToken = lens _uxmsrsChangeToken (\ s a -> s{_uxmsrsChangeToken = a});

-- | The response status code.
uxmsrsResponseStatus :: Lens' UpdateXSSMatchSetResponse Int
uxmsrsResponseStatus = lens _uxmsrsResponseStatus (\ s a -> s{_uxmsrsResponseStatus = a});

instance NFData UpdateXSSMatchSetResponse
