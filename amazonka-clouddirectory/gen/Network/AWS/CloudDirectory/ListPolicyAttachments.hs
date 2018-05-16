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
-- Module      : Network.AWS.CloudDirectory.ListPolicyAttachments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the @ObjectIdentifiers@ to which a given policy is attached.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudDirectory.ListPolicyAttachments
    (
    -- * Creating a Request
      listPolicyAttachments
    , ListPolicyAttachments
    -- * Request Lenses
    , lpaConsistencyLevel
    , lpaNextToken
    , lpaMaxResults
    , lpaDirectoryARN
    , lpaPolicyReference

    -- * Destructuring the Response
    , listPolicyAttachmentsResponse
    , ListPolicyAttachmentsResponse
    -- * Response Lenses
    , lparsObjectIdentifiers
    , lparsNextToken
    , lparsResponseStatus
    ) where

import Network.AWS.CloudDirectory.Types
import Network.AWS.CloudDirectory.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPolicyAttachments' smart constructor.
data ListPolicyAttachments = ListPolicyAttachments'
  { _lpaConsistencyLevel :: !(Maybe ConsistencyLevel)
  , _lpaNextToken        :: !(Maybe Text)
  , _lpaMaxResults       :: !(Maybe Nat)
  , _lpaDirectoryARN     :: !Text
  , _lpaPolicyReference  :: !ObjectReference
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicyAttachments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpaConsistencyLevel' - Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
--
-- * 'lpaNextToken' - The pagination token.
--
-- * 'lpaMaxResults' - The maximum number of items to be retrieved in a single call. This is an approximate number.
--
-- * 'lpaDirectoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- * 'lpaPolicyReference' - The reference that identifies the policy object.
listPolicyAttachments
    :: Text -- ^ 'lpaDirectoryARN'
    -> ObjectReference -- ^ 'lpaPolicyReference'
    -> ListPolicyAttachments
listPolicyAttachments pDirectoryARN_ pPolicyReference_ =
  ListPolicyAttachments'
    { _lpaConsistencyLevel = Nothing
    , _lpaNextToken = Nothing
    , _lpaMaxResults = Nothing
    , _lpaDirectoryARN = pDirectoryARN_
    , _lpaPolicyReference = pPolicyReference_
    }


-- | Represents the manner and timing in which the successful write or update of an object is reflected in a subsequent read operation of that same object.
lpaConsistencyLevel :: Lens' ListPolicyAttachments (Maybe ConsistencyLevel)
lpaConsistencyLevel = lens _lpaConsistencyLevel (\ s a -> s{_lpaConsistencyLevel = a})

-- | The pagination token.
lpaNextToken :: Lens' ListPolicyAttachments (Maybe Text)
lpaNextToken = lens _lpaNextToken (\ s a -> s{_lpaNextToken = a})

-- | The maximum number of items to be retrieved in a single call. This is an approximate number.
lpaMaxResults :: Lens' ListPolicyAttachments (Maybe Natural)
lpaMaxResults = lens _lpaMaxResults (\ s a -> s{_lpaMaxResults = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
lpaDirectoryARN :: Lens' ListPolicyAttachments Text
lpaDirectoryARN = lens _lpaDirectoryARN (\ s a -> s{_lpaDirectoryARN = a})

-- | The reference that identifies the policy object.
lpaPolicyReference :: Lens' ListPolicyAttachments ObjectReference
lpaPolicyReference = lens _lpaPolicyReference (\ s a -> s{_lpaPolicyReference = a})

instance AWSPager ListPolicyAttachments where
        page rq rs
          | stop (rs ^. lparsNextToken) = Nothing
          | stop (rs ^. lparsObjectIdentifiers) = Nothing
          | otherwise =
            Just $ rq & lpaNextToken .~ rs ^. lparsNextToken

instance AWSRequest ListPolicyAttachments where
        type Rs ListPolicyAttachments =
             ListPolicyAttachmentsResponse
        request = postJSON cloudDirectory
        response
          = receiveJSON
              (\ s h x ->
                 ListPolicyAttachmentsResponse' <$>
                   (x .?> "ObjectIdentifiers" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListPolicyAttachments where

instance NFData ListPolicyAttachments where

instance ToHeaders ListPolicyAttachments where
        toHeaders ListPolicyAttachments'{..}
          = mconcat
              ["x-amz-consistency-level" =# _lpaConsistencyLevel,
               "x-amz-data-partition" =# _lpaDirectoryARN]

instance ToJSON ListPolicyAttachments where
        toJSON ListPolicyAttachments'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lpaNextToken,
                  ("MaxResults" .=) <$> _lpaMaxResults,
                  Just ("PolicyReference" .= _lpaPolicyReference)])

instance ToPath ListPolicyAttachments where
        toPath
          = const
              "/amazonclouddirectory/2017-01-11/policy/attachment"

instance ToQuery ListPolicyAttachments where
        toQuery = const mempty

-- | /See:/ 'listPolicyAttachmentsResponse' smart constructor.
data ListPolicyAttachmentsResponse = ListPolicyAttachmentsResponse'
  { _lparsObjectIdentifiers :: !(Maybe [Text])
  , _lparsNextToken         :: !(Maybe Text)
  , _lparsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPolicyAttachmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lparsObjectIdentifiers' - A list of @ObjectIdentifiers@ to which the policy is attached.
--
-- * 'lparsNextToken' - The pagination token.
--
-- * 'lparsResponseStatus' - -- | The response status code.
listPolicyAttachmentsResponse
    :: Int -- ^ 'lparsResponseStatus'
    -> ListPolicyAttachmentsResponse
listPolicyAttachmentsResponse pResponseStatus_ =
  ListPolicyAttachmentsResponse'
    { _lparsObjectIdentifiers = Nothing
    , _lparsNextToken = Nothing
    , _lparsResponseStatus = pResponseStatus_
    }


-- | A list of @ObjectIdentifiers@ to which the policy is attached.
lparsObjectIdentifiers :: Lens' ListPolicyAttachmentsResponse [Text]
lparsObjectIdentifiers = lens _lparsObjectIdentifiers (\ s a -> s{_lparsObjectIdentifiers = a}) . _Default . _Coerce

-- | The pagination token.
lparsNextToken :: Lens' ListPolicyAttachmentsResponse (Maybe Text)
lparsNextToken = lens _lparsNextToken (\ s a -> s{_lparsNextToken = a})

-- | -- | The response status code.
lparsResponseStatus :: Lens' ListPolicyAttachmentsResponse Int
lparsResponseStatus = lens _lparsResponseStatus (\ s a -> s{_lparsResponseStatus = a})

instance NFData ListPolicyAttachmentsResponse where
