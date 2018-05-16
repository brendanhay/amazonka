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
-- Module      : Network.AWS.CloudDirectory.LookupPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all policies from the root of the 'Directory' to the object specified. If there are no policies present, an empty list is returned. If policies are present, and if some objects don't have the policies attached, it returns the @ObjectIdentifier@ for such objects. If policies are present, it returns @ObjectIdentifier@ , @policyId@ , and @policyType@ . Paths that don't lead to the root from the target object are ignored. For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.LookupPolicy
    (
    -- * Creating a Request
      lookupPolicy
    , LookupPolicy
    -- * Request Lenses
    , lpNextToken
    , lpMaxResults
    , lpDirectoryARN
    , lpObjectReference

    -- * Destructuring the Response
    , lookupPolicyResponse
    , LookupPolicyResponse
    -- * Response Lenses
    , lprsNextToken
    , lprsPolicyToPathList
    , lprsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'lookupPolicy' smart constructor.
data LookupPolicy = LookupPolicy'
  { _lpNextToken       :: !(Maybe Text)
  , _lpMaxResults      :: !(Maybe Nat)
  , _lpDirectoryARN    :: !Text
  , _lpObjectReference :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LookupPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpNextToken' - The token to request the next page of results.
--
-- * 'lpMaxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'lpDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
--
-- * 'lpObjectReference' - Reference that identifies the object whose policies will be looked up.
lookupPolicy
    :: Text -- ^ 'lpDirectoryARN'
    -> ObjectReference -- ^ 'lpObjectReference'
    -> LookupPolicy
lookupPolicy pDirectoryARN_ pObjectReference_ =
  LookupPolicy'
    { _lpNextToken = Nothing
    , _lpMaxResults = Nothing
    , _lpDirectoryARN = pDirectoryARN_
    , _lpObjectReference = pObjectReference_
    }


-- | The token to request the next page of results.
lpNextToken :: Lens' LookupPolicy (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a})

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
lpMaxResults :: Lens' LookupPolicy (Maybe Natural)
lpMaxResults = lens _lpMaxResults (\ s a -> s{_lpMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' . For more information, see 'arns' .
lpDirectoryARN :: Lens' LookupPolicy Text
lpDirectoryARN = lens _lpDirectoryARN (\ s a -> s{_lpDirectoryARN = a})

-- | Reference that identifies the object whose policies will be looked up.
lpObjectReference :: Lens' LookupPolicy ObjectReference
lpObjectReference = lens _lpObjectReference (\ s a -> s{_lpObjectReference = a})

instance AWSPager LookupPolicy where
        page rq rs
          | stop (rs ^. lprsNextToken) = Nothing
          | stop (rs ^. lprsPolicyToPathList) = Nothing
          | otherwise =
            Just $ rq & lpNextToken .~ rs ^. lprsNextToken

instance AWSRequest LookupPolicy where
        type Rs LookupPolicy = LookupPolicyResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 LookupPolicyResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "PolicyToPathList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable LookupPolicy where

instance NFData LookupPolicy where

instance ToHeaders LookupPolicy where
        toHeaders LookupPolicy'{..}
          = mconcat ["x-amz-data-partition" =# _lpDirectoryARN]

instance ToJSON LookupPolicy where
        toJSON LookupPolicy'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lpNextToken,
                  ("MaxResults" .=) <$> _lpMaxResults,
                  Just ("ObjectReference" .= _lpObjectReference)])

instance ToPath LookupPolicy where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/policy/lookup"

instance ToQuery LookupPolicy where
        toQuery = const mempty

-- | /See:/ 'lookupPolicyResponse' smart constructor.
data LookupPolicyResponse = LookupPolicyResponse'
  { _lprsNextToken        :: !(Maybe Text)
  , _lprsPolicyToPathList :: !(Maybe [PolicyToPath])
  , _lprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LookupPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsNextToken' - The pagination token.
--
-- * 'lprsPolicyToPathList' - Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .
--
-- * 'lprsResponseStatus' - -- | The response status code.
lookupPolicyResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> LookupPolicyResponse
lookupPolicyResponse pResponseStatus_ =
  LookupPolicyResponse'
    { _lprsNextToken = Nothing
    , _lprsPolicyToPathList = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | The pagination token.
lprsNextToken :: Lens' LookupPolicyResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a})

-- | Provides list of path to policies. Policies contain @PolicyId@ , @ObjectIdentifier@ , and @PolicyType@ . For more information, see <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/cd_key_concepts.html#policies Policies> .
lprsPolicyToPathList :: Lens' LookupPolicyResponse [PolicyToPath]
lprsPolicyToPathList = lens _lprsPolicyToPathList (\ s a -> s{_lprsPolicyToPathList = a}) . _Default . _Coerce

-- | -- | The response status code.
lprsResponseStatus :: Lens' LookupPolicyResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData LookupPolicyResponse where
