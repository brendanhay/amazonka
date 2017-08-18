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
-- Module      : Network.AWS.Greengrass.ListCoreDefinitionVersions
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists versions of a core definition.
module Network.AWS.Greengrass.ListCoreDefinitionVersions
    (
    -- * Creating a Request
      listCoreDefinitionVersions
    , ListCoreDefinitionVersions
    -- * Request Lenses
    , lcdvNextToken
    , lcdvMaxResults
    , lcdvCoreDefinitionId

    -- * Destructuring the Response
    , listCoreDefinitionVersionsResponse
    , ListCoreDefinitionVersionsResponse
    -- * Response Lenses
    , lcdvrsVersions
    , lcdvrsNextToken
    , lcdvrsResponseStatus
    ) where

import           Network.AWS.Greengrass.Types
import           Network.AWS.Greengrass.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'listCoreDefinitionVersions' smart constructor.
data ListCoreDefinitionVersions = ListCoreDefinitionVersions'
    { _lcdvNextToken        :: !(Maybe Text)
    , _lcdvMaxResults       :: !(Maybe Text)
    , _lcdvCoreDefinitionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListCoreDefinitionVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdvNextToken' - Specifies the pagination token used when iterating through a paginated request
--
-- * 'lcdvMaxResults' - Specifies the maximum number of list results to be returned in this page
--
-- * 'lcdvCoreDefinitionId' - core definition Id
listCoreDefinitionVersions
    :: Text -- ^ 'lcdvCoreDefinitionId'
    -> ListCoreDefinitionVersions
listCoreDefinitionVersions pCoreDefinitionId_ =
    ListCoreDefinitionVersions'
    { _lcdvNextToken = Nothing
    , _lcdvMaxResults = Nothing
    , _lcdvCoreDefinitionId = pCoreDefinitionId_
    }

-- | Specifies the pagination token used when iterating through a paginated request
lcdvNextToken :: Lens' ListCoreDefinitionVersions (Maybe Text)
lcdvNextToken = lens _lcdvNextToken (\ s a -> s{_lcdvNextToken = a});

-- | Specifies the maximum number of list results to be returned in this page
lcdvMaxResults :: Lens' ListCoreDefinitionVersions (Maybe Text)
lcdvMaxResults = lens _lcdvMaxResults (\ s a -> s{_lcdvMaxResults = a});

-- | core definition Id
lcdvCoreDefinitionId :: Lens' ListCoreDefinitionVersions Text
lcdvCoreDefinitionId = lens _lcdvCoreDefinitionId (\ s a -> s{_lcdvCoreDefinitionId = a});

instance AWSRequest ListCoreDefinitionVersions where
        type Rs ListCoreDefinitionVersions =
             ListCoreDefinitionVersionsResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 ListCoreDefinitionVersionsResponse' <$>
                   (x .?> "Versions" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListCoreDefinitionVersions

instance NFData ListCoreDefinitionVersions

instance ToHeaders ListCoreDefinitionVersions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListCoreDefinitionVersions where
        toPath ListCoreDefinitionVersions'{..}
          = mconcat
              ["/greengrass/definition/cores/",
               toBS _lcdvCoreDefinitionId, "/versions"]

instance ToQuery ListCoreDefinitionVersions where
        toQuery ListCoreDefinitionVersions'{..}
          = mconcat
              ["NextToken" =: _lcdvNextToken,
               "MaxResults" =: _lcdvMaxResults]

-- | /See:/ 'listCoreDefinitionVersionsResponse' smart constructor.
data ListCoreDefinitionVersionsResponse = ListCoreDefinitionVersionsResponse'
    { _lcdvrsVersions       :: !(Maybe [VersionInformation])
    , _lcdvrsNextToken      :: !(Maybe Text)
    , _lcdvrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListCoreDefinitionVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcdvrsVersions' - Versions
--
-- * 'lcdvrsNextToken' - The token for the next set of results, or ''null'' if there are no additional results.
--
-- * 'lcdvrsResponseStatus' - -- | The response status code.
listCoreDefinitionVersionsResponse
    :: Int -- ^ 'lcdvrsResponseStatus'
    -> ListCoreDefinitionVersionsResponse
listCoreDefinitionVersionsResponse pResponseStatus_ =
    ListCoreDefinitionVersionsResponse'
    { _lcdvrsVersions = Nothing
    , _lcdvrsNextToken = Nothing
    , _lcdvrsResponseStatus = pResponseStatus_
    }

-- | Versions
lcdvrsVersions :: Lens' ListCoreDefinitionVersionsResponse [VersionInformation]
lcdvrsVersions = lens _lcdvrsVersions (\ s a -> s{_lcdvrsVersions = a}) . _Default . _Coerce;

-- | The token for the next set of results, or ''null'' if there are no additional results.
lcdvrsNextToken :: Lens' ListCoreDefinitionVersionsResponse (Maybe Text)
lcdvrsNextToken = lens _lcdvrsNextToken (\ s a -> s{_lcdvrsNextToken = a});

-- | -- | The response status code.
lcdvrsResponseStatus :: Lens' ListCoreDefinitionVersionsResponse Int
lcdvrsResponseStatus = lens _lcdvrsResponseStatus (\ s a -> s{_lcdvrsResponseStatus = a});

instance NFData ListCoreDefinitionVersionsResponse
