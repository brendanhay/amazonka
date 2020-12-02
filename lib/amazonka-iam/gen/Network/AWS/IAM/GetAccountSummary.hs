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
-- Module      : Network.AWS.IAM.GetAccountSummary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about IAM entity usage and IAM quotas in the AWS account.
--
--
-- For information about limitations on IAM entities, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
--
module Network.AWS.IAM.GetAccountSummary
    (
    -- * Creating a Request
      getAccountSummary
    , GetAccountSummary

    -- * Destructuring the Response
    , getAccountSummaryResponse
    , GetAccountSummaryResponse
    -- * Response Lenses
    , gasrsSummaryMap
    , gasrsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getAccountSummary' smart constructor.
data GetAccountSummary =
  GetAccountSummary'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountSummary' with the minimum fields required to make a request.
--
getAccountSummary
    :: GetAccountSummary
getAccountSummary = GetAccountSummary'


instance AWSRequest GetAccountSummary where
        type Rs GetAccountSummary = GetAccountSummaryResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetAccountSummaryResult"
              (\ s h x ->
                 GetAccountSummaryResponse' <$>
                   (x .@? "SummaryMap" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure (fromEnum s)))

instance Hashable GetAccountSummary where

instance NFData GetAccountSummary where

instance ToHeaders GetAccountSummary where
        toHeaders = const mempty

instance ToPath GetAccountSummary where
        toPath = const "/"

instance ToQuery GetAccountSummary where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("GetAccountSummary" :: ByteString),
                  "Version" =: ("2010-05-08" :: ByteString)])

-- | Contains the response to a successful 'GetAccountSummary' request.
--
--
--
-- /See:/ 'getAccountSummaryResponse' smart constructor.
data GetAccountSummaryResponse = GetAccountSummaryResponse'
  { _gasrsSummaryMap     :: !(Maybe (Map SummaryKeyType Int))
  , _gasrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAccountSummaryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gasrsSummaryMap' - A set of key value pairs containing information about IAM entity usage and IAM quotas.
--
-- * 'gasrsResponseStatus' - -- | The response status code.
getAccountSummaryResponse
    :: Int -- ^ 'gasrsResponseStatus'
    -> GetAccountSummaryResponse
getAccountSummaryResponse pResponseStatus_ =
  GetAccountSummaryResponse'
    {_gasrsSummaryMap = Nothing, _gasrsResponseStatus = pResponseStatus_}


-- | A set of key value pairs containing information about IAM entity usage and IAM quotas.
gasrsSummaryMap :: Lens' GetAccountSummaryResponse (HashMap SummaryKeyType Int)
gasrsSummaryMap = lens _gasrsSummaryMap (\ s a -> s{_gasrsSummaryMap = a}) . _Default . _Map

-- | -- | The response status code.
gasrsResponseStatus :: Lens' GetAccountSummaryResponse Int
gasrsResponseStatus = lens _gasrsResponseStatus (\ s a -> s{_gasrsResponseStatus = a})

instance NFData GetAccountSummaryResponse where
