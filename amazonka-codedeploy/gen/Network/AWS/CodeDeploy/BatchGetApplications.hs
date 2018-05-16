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
-- Module      : Network.AWS.CodeDeploy.BatchGetApplications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more applications.
--
--
module Network.AWS.CodeDeploy.BatchGetApplications
    (
    -- * Creating a Request
      batchGetApplications
    , BatchGetApplications
    -- * Request Lenses
    , bgaApplicationNames

    -- * Destructuring the Response
    , batchGetApplicationsResponse
    , BatchGetApplicationsResponse
    -- * Response Lenses
    , bgarsApplicationsInfo
    , bgarsResponseStatus
    ) where

import Network.AWS.CodeDeploy.Types
import Network.AWS.CodeDeploy.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a BatchGetApplications operation.
--
--
--
-- /See:/ 'batchGetApplications' smart constructor.
newtype BatchGetApplications = BatchGetApplications'
  { _bgaApplicationNames :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetApplications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgaApplicationNames' - A list of application names separated by spaces.
batchGetApplications
    :: BatchGetApplications
batchGetApplications = BatchGetApplications' {_bgaApplicationNames = mempty}


-- | A list of application names separated by spaces.
bgaApplicationNames :: Lens' BatchGetApplications [Text]
bgaApplicationNames = lens _bgaApplicationNames (\ s a -> s{_bgaApplicationNames = a}) . _Coerce

instance AWSRequest BatchGetApplications where
        type Rs BatchGetApplications =
             BatchGetApplicationsResponse
        request = postJSON codeDeploy
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetApplicationsResponse' <$>
                   (x .?> "applicationsInfo" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable BatchGetApplications where

instance NFData BatchGetApplications where

instance ToHeaders BatchGetApplications where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.BatchGetApplications" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetApplications where
        toJSON BatchGetApplications'{..}
          = object
              (catMaybes
                 [Just ("applicationNames" .= _bgaApplicationNames)])

instance ToPath BatchGetApplications where
        toPath = const "/"

instance ToQuery BatchGetApplications where
        toQuery = const mempty

-- | Represents the output of a BatchGetApplications operation.
--
--
--
-- /See:/ 'batchGetApplicationsResponse' smart constructor.
data BatchGetApplicationsResponse = BatchGetApplicationsResponse'
  { _bgarsApplicationsInfo :: !(Maybe [ApplicationInfo])
  , _bgarsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetApplicationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgarsApplicationsInfo' - Information about the applications.
--
-- * 'bgarsResponseStatus' - -- | The response status code.
batchGetApplicationsResponse
    :: Int -- ^ 'bgarsResponseStatus'
    -> BatchGetApplicationsResponse
batchGetApplicationsResponse pResponseStatus_ =
  BatchGetApplicationsResponse'
    {_bgarsApplicationsInfo = Nothing, _bgarsResponseStatus = pResponseStatus_}


-- | Information about the applications.
bgarsApplicationsInfo :: Lens' BatchGetApplicationsResponse [ApplicationInfo]
bgarsApplicationsInfo = lens _bgarsApplicationsInfo (\ s a -> s{_bgarsApplicationsInfo = a}) . _Default . _Coerce

-- | -- | The response status code.
bgarsResponseStatus :: Lens' BatchGetApplicationsResponse Int
bgarsResponseStatus = lens _bgarsResponseStatus (\ s a -> s{_bgarsResponseStatus = a})

instance NFData BatchGetApplicationsResponse where
