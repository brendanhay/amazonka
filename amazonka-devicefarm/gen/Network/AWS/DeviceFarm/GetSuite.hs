{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.GetSuite
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a suite.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_GetSuite.html>
module Network.AWS.DeviceFarm.GetSuite
    (
    -- * Request
      GetSuite
    -- ** Request constructor
    , getSuite
    -- ** Request lenses
    , gsrqArn

    -- * Response
    , GetSuiteResponse
    -- ** Response constructor
    , getSuiteResponse
    -- ** Response lenses
    , gsrsSuite
    , gsrsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the get suite operation.
--
-- /See:/ 'getSuite' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsrqArn'
newtype GetSuite = GetSuite'
    { _gsrqArn :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetSuite' smart constructor.
getSuite :: Text -> GetSuite
getSuite pArn =
    GetSuite'
    { _gsrqArn = pArn
    }

-- | The suite\'s ARN.
gsrqArn :: Lens' GetSuite Text
gsrqArn = lens _gsrqArn (\ s a -> s{_gsrqArn = a});

instance AWSRequest GetSuite where
        type Sv GetSuite = DeviceFarm
        type Rs GetSuite = GetSuiteResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetSuiteResponse' <$>
                   (x .?> "suite") <*> (pure (fromEnum s)))

instance ToHeaders GetSuite where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.GetSuite" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetSuite where
        toJSON GetSuite'{..} = object ["arn" .= _gsrqArn]

instance ToPath GetSuite where
        toPath = const "/"

instance ToQuery GetSuite where
        toQuery = const mempty

-- | Represents the result of a get suite request.
--
-- /See:/ 'getSuiteResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsrsSuite'
--
-- * 'gsrsStatus'
data GetSuiteResponse = GetSuiteResponse'
    { _gsrsSuite  :: !(Maybe Suite)
    , _gsrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetSuiteResponse' smart constructor.
getSuiteResponse :: Int -> GetSuiteResponse
getSuiteResponse pStatus =
    GetSuiteResponse'
    { _gsrsSuite = Nothing
    , _gsrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
gsrsSuite :: Lens' GetSuiteResponse (Maybe Suite)
gsrsSuite = lens _gsrsSuite (\ s a -> s{_gsrsSuite = a});

-- | FIXME: Undocumented member.
gsrsStatus :: Lens' GetSuiteResponse Int
gsrsStatus = lens _gsrsStatus (\ s a -> s{_gsrsStatus = a});
