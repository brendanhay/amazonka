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
-- Module      : Network.AWS.SSM.ListAssociationVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all versions of an association for a specific association ID.
--
--
module Network.AWS.SSM.ListAssociationVersions
    (
    -- * Creating a Request
      listAssociationVersions
    , ListAssociationVersions
    -- * Request Lenses
    , lavNextToken
    , lavMaxResults
    , lavAssociationId

    -- * Destructuring the Response
    , listAssociationVersionsResponse
    , ListAssociationVersionsResponse
    -- * Response Lenses
    , lavrsNextToken
    , lavrsAssociationVersions
    , lavrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'listAssociationVersions' smart constructor.
data ListAssociationVersions = ListAssociationVersions'
  { _lavNextToken     :: !(Maybe Text)
  , _lavMaxResults    :: !(Maybe Nat)
  , _lavAssociationId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAssociationVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lavNextToken' - A token to start the list. Use this token to get the next set of results.
--
-- * 'lavMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'lavAssociationId' - The association ID for which you want to view all versions.
listAssociationVersions
    :: Text -- ^ 'lavAssociationId'
    -> ListAssociationVersions
listAssociationVersions pAssociationId_ =
  ListAssociationVersions'
    { _lavNextToken = Nothing
    , _lavMaxResults = Nothing
    , _lavAssociationId = pAssociationId_
    }


-- | A token to start the list. Use this token to get the next set of results.
lavNextToken :: Lens' ListAssociationVersions (Maybe Text)
lavNextToken = lens _lavNextToken (\ s a -> s{_lavNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
lavMaxResults :: Lens' ListAssociationVersions (Maybe Natural)
lavMaxResults = lens _lavMaxResults (\ s a -> s{_lavMaxResults = a}) . mapping _Nat

-- | The association ID for which you want to view all versions.
lavAssociationId :: Lens' ListAssociationVersions Text
lavAssociationId = lens _lavAssociationId (\ s a -> s{_lavAssociationId = a})

instance AWSRequest ListAssociationVersions where
        type Rs ListAssociationVersions =
             ListAssociationVersionsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ListAssociationVersionsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "AssociationVersions")
                     <*> (pure (fromEnum s)))

instance Hashable ListAssociationVersions where

instance NFData ListAssociationVersions where

instance ToHeaders ListAssociationVersions where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListAssociationVersions" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAssociationVersions where
        toJSON ListAssociationVersions'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lavNextToken,
                  ("MaxResults" .=) <$> _lavMaxResults,
                  Just ("AssociationId" .= _lavAssociationId)])

instance ToPath ListAssociationVersions where
        toPath = const "/"

instance ToQuery ListAssociationVersions where
        toQuery = const mempty

-- | /See:/ 'listAssociationVersionsResponse' smart constructor.
data ListAssociationVersionsResponse = ListAssociationVersionsResponse'
  { _lavrsNextToken           :: !(Maybe Text)
  , _lavrsAssociationVersions :: !(Maybe (List1 AssociationVersionInfo))
  , _lavrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAssociationVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lavrsNextToken' - The token for the next set of items to return. Use this token to get the next set of results.
--
-- * 'lavrsAssociationVersions' - Information about all versions of the association for the specified association ID.
--
-- * 'lavrsResponseStatus' - -- | The response status code.
listAssociationVersionsResponse
    :: Int -- ^ 'lavrsResponseStatus'
    -> ListAssociationVersionsResponse
listAssociationVersionsResponse pResponseStatus_ =
  ListAssociationVersionsResponse'
    { _lavrsNextToken = Nothing
    , _lavrsAssociationVersions = Nothing
    , _lavrsResponseStatus = pResponseStatus_
    }


-- | The token for the next set of items to return. Use this token to get the next set of results.
lavrsNextToken :: Lens' ListAssociationVersionsResponse (Maybe Text)
lavrsNextToken = lens _lavrsNextToken (\ s a -> s{_lavrsNextToken = a})

-- | Information about all versions of the association for the specified association ID.
lavrsAssociationVersions :: Lens' ListAssociationVersionsResponse (Maybe (NonEmpty AssociationVersionInfo))
lavrsAssociationVersions = lens _lavrsAssociationVersions (\ s a -> s{_lavrsAssociationVersions = a}) . mapping _List1

-- | -- | The response status code.
lavrsResponseStatus :: Lens' ListAssociationVersionsResponse Int
lavrsResponseStatus = lens _lavrsResponseStatus (\ s a -> s{_lavrsResponseStatus = a})

instance NFData ListAssociationVersionsResponse where
