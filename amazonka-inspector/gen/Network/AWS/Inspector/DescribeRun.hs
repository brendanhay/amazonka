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
-- Module      : Network.AWS.Inspector.DescribeRun
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the assessment run specified by the run ARN.
module Network.AWS.Inspector.DescribeRun
    (
    -- * Creating a Request
      describeRun
    , DescribeRun
    -- * Request Lenses
    , drRunARN

    -- * Destructuring the Response
    , describeRunResponse
    , DescribeRunResponse
    -- * Response Lenses
    , drrsRun
    , drrsResponseStatus
    ) where

import           Network.AWS.Inspector.Types
import           Network.AWS.Inspector.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeRun' smart constructor.
newtype DescribeRun = DescribeRun'
    { _drRunARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drRunARN'
describeRun
    :: Text -- ^ 'drRunARN'
    -> DescribeRun
describeRun pRunARN_ =
    DescribeRun'
    { _drRunARN = pRunARN_
    }

-- | The ARN specifying the assessment run that you want to describe.
drRunARN :: Lens' DescribeRun Text
drRunARN = lens _drRunARN (\ s a -> s{_drRunARN = a});

instance AWSRequest DescribeRun where
        type Rs DescribeRun = DescribeRunResponse
        request = postJSON inspector
        response
          = receiveJSON
              (\ s h x ->
                 DescribeRunResponse' <$>
                   (x .?> "run") <*> (pure (fromEnum s)))

instance ToHeaders DescribeRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("InspectorService.DescribeRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeRun where
        toJSON DescribeRun'{..}
          = object (catMaybes [Just ("runArn" .= _drRunARN)])

instance ToPath DescribeRun where
        toPath = const "/"

instance ToQuery DescribeRun where
        toQuery = const mempty

-- | /See:/ 'describeRunResponse' smart constructor.
data DescribeRunResponse = DescribeRunResponse'
    { _drrsRun            :: !(Maybe Run)
    , _drrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeRunResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drrsRun'
--
-- * 'drrsResponseStatus'
describeRunResponse
    :: Int -- ^ 'drrsResponseStatus'
    -> DescribeRunResponse
describeRunResponse pResponseStatus_ =
    DescribeRunResponse'
    { _drrsRun = Nothing
    , _drrsResponseStatus = pResponseStatus_
    }

-- | Information about the assessment run.
drrsRun :: Lens' DescribeRunResponse (Maybe Run)
drrsRun = lens _drrsRun (\ s a -> s{_drrsRun = a});

-- | The response status code.
drrsResponseStatus :: Lens' DescribeRunResponse Int
drrsResponseStatus = lens _drrsResponseStatus (\ s a -> s{_drrsResponseStatus = a});
