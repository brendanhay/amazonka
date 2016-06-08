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
-- Module      : Network.AWS.WAF.UpdateSizeConstraintSet
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Inserts or deletes < SizeConstraint> objects (filters) in a < SizeConstraintSet>. For each 'SizeConstraint' object, you specify the following values:
--
-- -   Whether to insert or delete the object from the array. If you want to change a 'SizeConstraintSetUpdate' object, you delete the existing object and add a new one.
-- -   The part of a web request that you want AWS WAF to evaluate, such as the length of a query string or the length of the 'User-Agent' header.
-- -   Whether to perform any transformations on the request, such as converting it to lowercase, before checking its length. Note that transformations of the request body are not supported because the AWS resource forwards only the first '8192' bytes of your request to AWS WAF.
-- -   A 'ComparisonOperator' used for evaluating the selected part of the request against the specified 'Size', such as equals, greater than, less than, and so on.
-- -   The length, in bytes, that you want AWS WAF to watch for in selected part of the request. The length is computed after applying the transformation.
--
-- For example, you can add a 'SizeConstraintSetUpdate' object that matches web requests in which the length of the 'User-Agent' header is greater than 100 bytes. You can then configure AWS WAF to block those requests.
--
-- To create and configure a 'SizeConstraintSet', perform the following steps:
--
-- 1.  Create a 'SizeConstraintSet.' For more information, see < CreateSizeConstraintSet>.
-- 2.  Use < GetChangeToken> to get the change token that you provide in the 'ChangeToken' parameter of an 'UpdateSizeConstraintSet' request.
-- 3.  Submit an 'UpdateSizeConstraintSet' request to specify the part of the request that you want AWS WAF to inspect (for example, the header or the URI) and the value that you want AWS WAF to watch for.
--
-- For more information about how to use the AWS WAF API to allow or block HTTP requests, see the <http://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF Developer Guide>.
module Network.AWS.WAF.UpdateSizeConstraintSet
    (
    -- * Creating a Request
      updateSizeConstraintSet
    , UpdateSizeConstraintSet
    -- * Request Lenses
    , uscsSizeConstraintSetId
    , uscsChangeToken
    , uscsUpdates

    -- * Destructuring the Response
    , updateSizeConstraintSetResponse
    , UpdateSizeConstraintSetResponse
    -- * Response Lenses
    , uscsrsChangeToken
    , uscsrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.WAF.Types
import           Network.AWS.WAF.Types.Product

-- | /See:/ 'updateSizeConstraintSet' smart constructor.
data UpdateSizeConstraintSet = UpdateSizeConstraintSet'
    { _uscsSizeConstraintSetId :: !Text
    , _uscsChangeToken         :: !Text
    , _uscsUpdates             :: ![SizeConstraintSetUpdate]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateSizeConstraintSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscsSizeConstraintSetId'
--
-- * 'uscsChangeToken'
--
-- * 'uscsUpdates'
updateSizeConstraintSet
    :: Text -- ^ 'uscsSizeConstraintSetId'
    -> Text -- ^ 'uscsChangeToken'
    -> UpdateSizeConstraintSet
updateSizeConstraintSet pSizeConstraintSetId_ pChangeToken_ =
    UpdateSizeConstraintSet'
    { _uscsSizeConstraintSetId = pSizeConstraintSetId_
    , _uscsChangeToken = pChangeToken_
    , _uscsUpdates = mempty
    }

-- | The 'SizeConstraintSetId' of the < SizeConstraintSet> that you want to update. 'SizeConstraintSetId' is returned by < CreateSizeConstraintSet> and by < ListSizeConstraintSets>.
uscsSizeConstraintSetId :: Lens' UpdateSizeConstraintSet Text
uscsSizeConstraintSetId = lens _uscsSizeConstraintSetId (\ s a -> s{_uscsSizeConstraintSetId = a});

-- | The value returned by the most recent call to < GetChangeToken>.
uscsChangeToken :: Lens' UpdateSizeConstraintSet Text
uscsChangeToken = lens _uscsChangeToken (\ s a -> s{_uscsChangeToken = a});

-- | An array of 'SizeConstraintSetUpdate' objects that you want to insert into or delete from a < SizeConstraintSet>. For more information, see the applicable data types:
--
-- -   < SizeConstraintSetUpdate>: Contains 'Action' and 'SizeConstraint'
-- -   < SizeConstraint>: Contains 'FieldToMatch', 'TextTransformation', 'ComparisonOperator', and 'Size'
-- -   < FieldToMatch>: Contains 'Data' and 'Type'
uscsUpdates :: Lens' UpdateSizeConstraintSet [SizeConstraintSetUpdate]
uscsUpdates = lens _uscsUpdates (\ s a -> s{_uscsUpdates = a}) . _Coerce;

instance AWSRequest UpdateSizeConstraintSet where
        type Rs UpdateSizeConstraintSet =
             UpdateSizeConstraintSetResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSizeConstraintSetResponse' <$>
                   (x .?> "ChangeToken") <*> (pure (fromEnum s)))

instance Hashable UpdateSizeConstraintSet

instance NFData UpdateSizeConstraintSet

instance ToHeaders UpdateSizeConstraintSet where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.UpdateSizeConstraintSet" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSizeConstraintSet where
        toJSON UpdateSizeConstraintSet'{..}
          = object
              (catMaybes
                 [Just
                    ("SizeConstraintSetId" .= _uscsSizeConstraintSetId),
                  Just ("ChangeToken" .= _uscsChangeToken),
                  Just ("Updates" .= _uscsUpdates)])

instance ToPath UpdateSizeConstraintSet where
        toPath = const "/"

instance ToQuery UpdateSizeConstraintSet where
        toQuery = const mempty

-- | /See:/ 'updateSizeConstraintSetResponse' smart constructor.
data UpdateSizeConstraintSetResponse = UpdateSizeConstraintSetResponse'
    { _uscsrsChangeToken    :: !(Maybe Text)
    , _uscsrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateSizeConstraintSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uscsrsChangeToken'
--
-- * 'uscsrsResponseStatus'
updateSizeConstraintSetResponse
    :: Int -- ^ 'uscsrsResponseStatus'
    -> UpdateSizeConstraintSetResponse
updateSizeConstraintSetResponse pResponseStatus_ =
    UpdateSizeConstraintSetResponse'
    { _uscsrsChangeToken = Nothing
    , _uscsrsResponseStatus = pResponseStatus_
    }

-- | The 'ChangeToken' that you used to submit the 'UpdateSizeConstraintSet' request. You can also use this value to query the status of the request. For more information, see < GetChangeTokenStatus>.
uscsrsChangeToken :: Lens' UpdateSizeConstraintSetResponse (Maybe Text)
uscsrsChangeToken = lens _uscsrsChangeToken (\ s a -> s{_uscsrsChangeToken = a});

-- | The response status code.
uscsrsResponseStatus :: Lens' UpdateSizeConstraintSetResponse Int
uscsrsResponseStatus = lens _uscsrsResponseStatus (\ s a -> s{_uscsrsResponseStatus = a});

instance NFData UpdateSizeConstraintSetResponse
