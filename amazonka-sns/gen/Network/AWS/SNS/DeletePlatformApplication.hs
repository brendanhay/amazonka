{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.DeletePlatformApplication
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Deletes a platform application object for one of the supported push
-- notification services, such as APNS and GCM. For more information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_DeletePlatformApplication.html>
module Network.AWS.SNS.DeletePlatformApplication
    (
    -- * Request
      DeletePlatformApplication
    -- ** Request constructor
    , deletePlatformApplication
    -- ** Request lenses
    , dpaPlatformApplicationARN

    -- * Response
    , DeletePlatformApplicationResponse
    -- ** Response constructor
    , deletePlatformApplicationResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for DeletePlatformApplication action.
--
-- /See:/ 'deletePlatformApplication' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpaPlatformApplicationARN'
newtype DeletePlatformApplication = DeletePlatformApplication'
    { _dpaPlatformApplicationARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePlatformApplication' smart constructor.
deletePlatformApplication :: Text -> DeletePlatformApplication
deletePlatformApplication pPlatformApplicationARN =
    DeletePlatformApplication'
    { _dpaPlatformApplicationARN = pPlatformApplicationARN
    }

-- | PlatformApplicationArn of platform application object to delete.
dpaPlatformApplicationARN :: Lens' DeletePlatformApplication Text
dpaPlatformApplicationARN = lens _dpaPlatformApplicationARN (\ s a -> s{_dpaPlatformApplicationARN = a});

instance AWSRequest DeletePlatformApplication where
        type Sv DeletePlatformApplication = SNS
        type Rs DeletePlatformApplication =
             DeletePlatformApplicationResponse
        request = post
        response
          = receiveNull DeletePlatformApplicationResponse'

instance ToHeaders DeletePlatformApplication where
        toHeaders = const mempty

instance ToPath DeletePlatformApplication where
        toPath = const "/"

instance ToQuery DeletePlatformApplication where
        toQuery DeletePlatformApplication'{..}
          = mconcat
              ["Action" =:
                 ("DeletePlatformApplication" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "PlatformApplicationArn" =:
                 _dpaPlatformApplicationARN]

-- | /See:/ 'deletePlatformApplicationResponse' smart constructor.
data DeletePlatformApplicationResponse =
    DeletePlatformApplicationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeletePlatformApplicationResponse' smart constructor.
deletePlatformApplicationResponse :: DeletePlatformApplicationResponse
deletePlatformApplicationResponse = DeletePlatformApplicationResponse'
