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
-- Module      : Network.AWS.CodeDeploy.ListApplications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the applications registered with the applicable IAM user or AWS account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CodeDeploy.ListApplications
    (
    -- * Creating a Request
      listApplications
    , ListApplications
    -- * Request Lenses
    , laNextToken

    -- * Destructuring the Response
    , listApplicationsResponse
    , ListApplicationsResponse
    -- * Response Lenses
    , larsNextToken
    , larsApplications
    , larsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a ListApplications operation.
--
--
--
-- /See:/ 'listApplications' smart constructor.
newtype ListApplications = ListApplications'
  { _laNextToken :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListApplications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNextToken' - An identifier returned from the previous list applications call. It can be used to return the next set of applications in the list.
listApplications
    :: ListApplications
listApplications = ListApplications' {_laNextToken = Nothing}


-- | An identifier returned from the previous list applications call. It can be used to return the next set of applications in the list.
laNextToken :: Lens' ListApplications (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a})

instance AWSPager ListApplications where
        page rq rs
          | stop (rs ^. larsNextToken) = Nothing
          | stop (rs ^. larsApplications) = Nothing
          | otherwise =
            Just $ rq & laNextToken .~ rs ^. larsNextToken

instance AWSRequest ListApplications where
        type Rs ListApplications = ListApplicationsResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 ListApplicationsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "applications" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListApplications where

instance NFData ListApplications where

instance ToHeaders ListApplications where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.ListApplications" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListApplications where
        toJSON ListApplications'{..}
          = object
              (catMaybes [("nextToken" .=) <$> _laNextToken])

instance ToPath ListApplications where
        toPath = const "/"

instance ToQuery ListApplications where
        toQuery = const mempty

-- | Represents the output of a ListApplications operation.
--
--
--
-- /See:/ 'listApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
  { _larsNextToken      :: !(Maybe Text)
  , _larsApplications   :: !(Maybe [Text])
  , _larsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsNextToken' - If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list applications call to return the next set of applications, will also be returned. in the list.
--
-- * 'larsApplications' - A list of application names.
--
-- * 'larsResponseStatus' - -- | The response status code.
listApplicationsResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListApplicationsResponse
listApplicationsResponse pResponseStatus_ =
  ListApplicationsResponse'
    { _larsNextToken = Nothing
    , _larsApplications = Nothing
    , _larsResponseStatus = pResponseStatus_
    }


-- | If a large amount of information is returned, an identifier is also returned. It can be used in a subsequent list applications call to return the next set of applications, will also be returned. in the list.
larsNextToken :: Lens' ListApplicationsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a})

-- | A list of application names.
larsApplications :: Lens' ListApplicationsResponse [Text]
larsApplications = lens _larsApplications (\ s a -> s{_larsApplications = a}) . _Default . _Coerce

-- | -- | The response status code.
larsResponseStatus :: Lens' ListApplicationsResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

instance NFData ListApplicationsResponse where
