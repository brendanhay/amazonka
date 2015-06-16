{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.DeleteHapg
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

-- | Deletes a high-availability partition group.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DeleteHapg.html>
module Network.AWS.CloudHSM.DeleteHapg
    (
    -- * Request
      DeleteHapg
    -- ** Request constructor
    , deleteHapg
    -- ** Request lenses
    , dhHapgARN

    -- * Response
    , DeleteHapgResponse
    -- ** Response constructor
    , deleteHapgResponse
    -- ** Response lenses
    , dhrStatus
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.CloudHSM.Types

-- | /See:/ 'deleteHapg' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhHapgARN'
newtype DeleteHapg = DeleteHapg'{_dhHapgARN :: Text} deriving (Eq, Read, Show)

-- | 'DeleteHapg' smart constructor.
deleteHapg :: Text -> DeleteHapg
deleteHapg pHapgARN = DeleteHapg'{_dhHapgARN = pHapgARN};

-- | The ARN of the high-availability partition group to delete.
dhHapgARN :: Lens' DeleteHapg Text
dhHapgARN = lens _dhHapgARN (\ s a -> s{_dhHapgARN = a});

instance AWSRequest DeleteHapg where
        type Sv DeleteHapg = CloudHSM
        type Rs DeleteHapg = DeleteHapgResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x -> DeleteHapgResponse' <$> (x .:> "Status"))

instance ToHeaders DeleteHapg where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DeleteHapg" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteHapg where
        toJSON DeleteHapg'{..}
          = object ["HapgArn" .= _dhHapgARN]

instance ToPath DeleteHapg where
        toPath = const "/"

instance ToQuery DeleteHapg where
        toQuery = const mempty

-- | /See:/ 'deleteHapgResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhrStatus'
newtype DeleteHapgResponse = DeleteHapgResponse'{_dhrStatus :: Text} deriving (Eq, Read, Show)

-- | 'DeleteHapgResponse' smart constructor.
deleteHapgResponse :: Text -> DeleteHapgResponse
deleteHapgResponse pStatus = DeleteHapgResponse'{_dhrStatus = pStatus};

-- | The status of the action.
dhrStatus :: Lens' DeleteHapgResponse Text
dhrStatus = lens _dhrStatus (\ s a -> s{_dhrStatus = a});
