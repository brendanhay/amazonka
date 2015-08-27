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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the applications registered with the applicable IAM user or AWS
-- account.
--
-- /See:/ <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_ListApplications.html AWS API Reference> for ListApplications.
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
    , larsStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.CodeDeploy.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a list applications operation.
--
-- /See:/ 'listApplications' smart constructor.
newtype ListApplications = ListApplications'
    { _laNextToken :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListApplications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNextToken'
listApplications
    :: ListApplications
listApplications =
    ListApplications'
    { _laNextToken = Nothing
    }

-- | An identifier that was returned from the previous list applications
-- call, which can be used to return the next set of applications in the
-- list.
laNextToken :: Lens' ListApplications (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a});

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

-- | Represents the output of a list applications operation.
--
-- /See:/ 'listApplicationsResponse' smart constructor.
data ListApplicationsResponse = ListApplicationsResponse'
    { _larsNextToken    :: !(Maybe Text)
    , _larsApplications :: !(Maybe [Text])
    , _larsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsNextToken'
--
-- * 'larsApplications'
--
-- * 'larsStatus'
listApplicationsResponse
    :: Int -- ^ 'larsStatus'
    -> ListApplicationsResponse
listApplicationsResponse pStatus_ =
    ListApplicationsResponse'
    { _larsNextToken = Nothing
    , _larsApplications = Nothing
    , _larsStatus = pStatus_
    }

-- | If the amount of information that is returned is significantly large, an
-- identifier will also be returned, which can be used in a subsequent list
-- applications call to return the next set of applications in the list.
larsNextToken :: Lens' ListApplicationsResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a});

-- | A list of application names.
larsApplications :: Lens' ListApplicationsResponse [Text]
larsApplications = lens _larsApplications (\ s a -> s{_larsApplications = a}) . _Default . _Coerce;

-- | The response status code.
larsStatus :: Lens' ListApplicationsResponse Int
larsStatus = lens _larsStatus (\ s a -> s{_larsStatus = a});
