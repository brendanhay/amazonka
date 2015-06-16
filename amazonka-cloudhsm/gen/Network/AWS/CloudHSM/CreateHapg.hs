{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.CreateHapg
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a high-availability partition group. A high-availability
-- partition group is a group of partitions that spans multiple physical
-- HSMs.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_CreateHapg.html>
module Network.AWS.CloudHSM.CreateHapg
    (
    -- * Request
      CreateHapg
    -- ** Request constructor
    , createHapg
    -- ** Request lenses
    , chLabel

    -- * Response
    , CreateHapgResponse
    -- ** Response constructor
    , createHapgResponse
    -- ** Response lenses
    , chrHapgARN
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudHSM.Types

-- | /See:/ 'createHapg' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chLabel'
newtype CreateHapg = CreateHapg'{_chLabel :: Text} deriving (Eq, Read, Show)

-- | 'CreateHapg' smart constructor.
createHapg :: Text -> CreateHapg
createHapg pLabel = CreateHapg'{_chLabel = pLabel};

-- | The label of the new high-availability partition group.
chLabel :: Lens' CreateHapg Text
chLabel = lens _chLabel (\ s a -> s{_chLabel = a});

instance AWSRequest CreateHapg where
        type Sv CreateHapg = CloudHSM
        type Rs CreateHapg = CreateHapgResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateHapgResponse' <$> (x .?> "HapgArn"))

instance ToHeaders CreateHapg where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.CreateHapg" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHapg where
        toJSON CreateHapg'{..} = object ["Label" .= _chLabel]

instance ToPath CreateHapg where
        toPath = const "/"

instance ToQuery CreateHapg where
        toQuery = const mempty

-- | /See:/ 'createHapgResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chrHapgARN'
newtype CreateHapgResponse = CreateHapgResponse'{_chrHapgARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'CreateHapgResponse' smart constructor.
createHapgResponse :: CreateHapgResponse
createHapgResponse = CreateHapgResponse'{_chrHapgARN = Nothing};

-- | The ARN of the high-availability partition group.
chrHapgARN :: Lens' CreateHapgResponse (Maybe Text)
chrHapgARN = lens _chrHapgARN (\ s a -> s{_chrHapgARN = a});
