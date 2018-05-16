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
-- Module      : Network.AWS.SSM.ListDocumentVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all versions for a document.
--
--
module Network.AWS.SSM.ListDocumentVersions
    (
    -- * Creating a Request
      listDocumentVersions
    , ListDocumentVersions
    -- * Request Lenses
    , ldvNextToken
    , ldvMaxResults
    , ldvName

    -- * Destructuring the Response
    , listDocumentVersionsResponse
    , ListDocumentVersionsResponse
    -- * Response Lenses
    , ldvrsDocumentVersions
    , ldvrsNextToken
    , ldvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'listDocumentVersions' smart constructor.
data ListDocumentVersions = ListDocumentVersions'
  { _ldvNextToken  :: !(Maybe Text)
  , _ldvMaxResults :: !(Maybe Nat)
  , _ldvName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDocumentVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldvNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'ldvMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'ldvName' - The name of the document about which you want version information.
listDocumentVersions
    :: Text -- ^ 'ldvName'
    -> ListDocumentVersions
listDocumentVersions pName_ =
  ListDocumentVersions'
    {_ldvNextToken = Nothing, _ldvMaxResults = Nothing, _ldvName = pName_}


-- | The token for the next set of items to return. (You received this token from a previous call.)
ldvNextToken :: Lens' ListDocumentVersions (Maybe Text)
ldvNextToken = lens _ldvNextToken (\ s a -> s{_ldvNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
ldvMaxResults :: Lens' ListDocumentVersions (Maybe Natural)
ldvMaxResults = lens _ldvMaxResults (\ s a -> s{_ldvMaxResults = a}) . mapping _Nat

-- | The name of the document about which you want version information.
ldvName :: Lens' ListDocumentVersions Text
ldvName = lens _ldvName (\ s a -> s{_ldvName = a})

instance AWSRequest ListDocumentVersions where
        type Rs ListDocumentVersions =
             ListDocumentVersionsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ListDocumentVersionsResponse' <$>
                   (x .?> "DocumentVersions") <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListDocumentVersions where

instance NFData ListDocumentVersions where

instance ToHeaders ListDocumentVersions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListDocumentVersions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListDocumentVersions where
        toJSON ListDocumentVersions'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ldvNextToken,
                  ("MaxResults" .=) <$> _ldvMaxResults,
                  Just ("Name" .= _ldvName)])

instance ToPath ListDocumentVersions where
        toPath = const "/"

instance ToQuery ListDocumentVersions where
        toQuery = const mempty

-- | /See:/ 'listDocumentVersionsResponse' smart constructor.
data ListDocumentVersionsResponse = ListDocumentVersionsResponse'
  { _ldvrsDocumentVersions :: !(Maybe (List1 DocumentVersionInfo))
  , _ldvrsNextToken        :: !(Maybe Text)
  , _ldvrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDocumentVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldvrsDocumentVersions' - The document versions.
--
-- * 'ldvrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'ldvrsResponseStatus' - -- | The response status code.
listDocumentVersionsResponse
    :: Int -- ^ 'ldvrsResponseStatus'
    -> ListDocumentVersionsResponse
listDocumentVersionsResponse pResponseStatus_ =
  ListDocumentVersionsResponse'
    { _ldvrsDocumentVersions = Nothing
    , _ldvrsNextToken = Nothing
    , _ldvrsResponseStatus = pResponseStatus_
    }


-- | The document versions.
ldvrsDocumentVersions :: Lens' ListDocumentVersionsResponse (Maybe (NonEmpty DocumentVersionInfo))
ldvrsDocumentVersions = lens _ldvrsDocumentVersions (\ s a -> s{_ldvrsDocumentVersions = a}) . mapping _List1

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
ldvrsNextToken :: Lens' ListDocumentVersionsResponse (Maybe Text)
ldvrsNextToken = lens _ldvrsNextToken (\ s a -> s{_ldvrsNextToken = a})

-- | -- | The response status code.
ldvrsResponseStatus :: Lens' ListDocumentVersionsResponse Int
ldvrsResponseStatus = lens _ldvrsResponseStatus (\ s a -> s{_ldvrsResponseStatus = a})

instance NFData ListDocumentVersionsResponse where
