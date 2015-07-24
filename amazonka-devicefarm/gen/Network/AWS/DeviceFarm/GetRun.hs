{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetRun
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a run.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetRun.html>
module Network.AWS.DeviceFarm.GetRun
    (
    -- * Request
      GetRun
    -- ** Request constructor
    , getRun
    -- ** Request lenses
    , grArn

    -- * Response
    , GetRunResponse
    -- ** Response constructor
    , getRunResponse
    -- ** Response lenses
    , grrsRun
    , grrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get run operation.
--
-- /See:/ 'getRun' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grArn'
newtype GetRun = GetRun'
    { _grArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRun' smart constructor.
getRun :: Text -> GetRun
getRun pArn_ =
    GetRun'
    { _grArn = pArn_
    }

-- | The run\'s ARN.
grArn :: Lens' GetRun Text
grArn = lens _grArn (\ s a -> s{_grArn = a});

instance AWSRequest GetRun where
        type Sv GetRun = DeviceFarm
        type Rs GetRun = GetRunResponse
        request = postJSON "GetRun"
        response
          = receiveJSON
              (\ s h x ->
                 GetRunResponse' <$>
                   (x .?> "run") <*> (pure (fromEnum s)))

instance ToHeaders GetRun where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetRun" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetRun where
        toJSON GetRun'{..} = object ["arn" .= _grArn]

instance ToPath GetRun where
        toPath = const "/"

instance ToQuery GetRun where
        toQuery = const mempty

-- | Represents the result of a get run request.
--
-- /See:/ 'getRunResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrsRun'
--
-- * 'grrsStatus'
data GetRunResponse = GetRunResponse'
    { _grrsRun    :: !(Maybe Run)
    , _grrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRunResponse' smart constructor.
getRunResponse :: Int -> GetRunResponse
getRunResponse pStatus_ =
    GetRunResponse'
    { _grrsRun = Nothing
    , _grrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
grrsRun :: Lens' GetRunResponse (Maybe Run)
grrsRun = lens _grrsRun (\ s a -> s{_grrsRun = a});

-- | FIXME: Undocumented member.
grrsStatus :: Lens' GetRunResponse Int
grrsStatus = lens _grrsStatus (\ s a -> s{_grrsStatus = a});
