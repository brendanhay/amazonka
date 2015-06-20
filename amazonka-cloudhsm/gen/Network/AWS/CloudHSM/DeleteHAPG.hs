{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudHSM.DeleteHAPG
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
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_DeleteHAPG.html>
module Network.AWS.CloudHSM.DeleteHAPG
    (
    -- * Request
      DeleteHAPG
    -- ** Request constructor
    , deleteHAPG
    -- ** Request lenses
    , dhHAPGARN

    -- * Response
    , DeleteHAPGResponse
    -- ** Response constructor
    , deleteHAPGResponse
    -- ** Response lenses
    , dhrStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteHAPG' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhHAPGARN'
newtype DeleteHAPG = DeleteHAPG'{_dhHAPGARN :: Text} deriving (Eq, Read, Show)

-- | 'DeleteHAPG' smart constructor.
deleteHAPG :: Text -> DeleteHAPG
deleteHAPG pHAPGARN = DeleteHAPG'{_dhHAPGARN = pHAPGARN};

-- | The ARN of the high-availability partition group to delete.
dhHAPGARN :: Lens' DeleteHAPG Text
dhHAPGARN = lens _dhHAPGARN (\ s a -> s{_dhHAPGARN = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteHAPG where
        type Sv DeleteHAPG = CloudHSM
        type Rs DeleteHAPG = DeleteHAPGResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x -> DeleteHAPGResponse' <$> (x .:> "Status"))

instance ToHeaders DeleteHAPG where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.DeleteHAPG" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteHAPG where
        toJSON DeleteHAPG'{..}
          = object ["HapgArn" .= _dhHAPGARN]

instance ToPath DeleteHAPG where
        toPath = const "/"

instance ToQuery DeleteHAPG where
        toQuery = const mempty

-- | /See:/ 'deleteHAPGResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhrStatus'
newtype DeleteHAPGResponse = DeleteHAPGResponse'{_dhrStatus :: Text} deriving (Eq, Read, Show)

-- | 'DeleteHAPGResponse' smart constructor.
deleteHAPGResponse :: Text -> DeleteHAPGResponse
deleteHAPGResponse pStatus = DeleteHAPGResponse'{_dhrStatus = pStatus};

-- | The status of the action.
dhrStatus :: Lens' DeleteHAPGResponse Text
dhrStatus = lens _dhrStatus (\ s a -> s{_dhrStatus = a});
