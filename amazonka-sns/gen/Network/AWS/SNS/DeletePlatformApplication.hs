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
-- Module      : Network.AWS.SNS.DeletePlatformApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a platform application object for one of the supported push notification services, such as APNS and GCM. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> .
--
--
module Network.AWS.SNS.DeletePlatformApplication
    (
    -- * Creating a Request
      deletePlatformApplication
    , DeletePlatformApplication
    -- * Request Lenses
    , dpaPlatformApplicationARN

    -- * Destructuring the Response
    , deletePlatformApplicationResponse
    , DeletePlatformApplicationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for DeletePlatformApplication action.
--
--
--
-- /See:/ 'deletePlatformApplication' smart constructor.
newtype DeletePlatformApplication = DeletePlatformApplication'
  { _dpaPlatformApplicationARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePlatformApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpaPlatformApplicationARN' - PlatformApplicationArn of platform application object to delete.
deletePlatformApplication
    :: Text -- ^ 'dpaPlatformApplicationARN'
    -> DeletePlatformApplication
deletePlatformApplication pPlatformApplicationARN_ =
  DeletePlatformApplication'
    {_dpaPlatformApplicationARN = pPlatformApplicationARN_}


-- | PlatformApplicationArn of platform application object to delete.
dpaPlatformApplicationARN :: Lens' DeletePlatformApplication Text
dpaPlatformApplicationARN = lens _dpaPlatformApplicationARN (\ s a -> s{_dpaPlatformApplicationARN = a})

instance AWSRequest DeletePlatformApplication where
        type Rs DeletePlatformApplication =
             DeletePlatformApplicationResponse
        request = postQuery sns
        response
          = receiveNull DeletePlatformApplicationResponse'

instance Hashable DeletePlatformApplication where

instance NFData DeletePlatformApplication where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePlatformApplicationResponse' with the minimum fields required to make a request.
--
deletePlatformApplicationResponse
    :: DeletePlatformApplicationResponse
deletePlatformApplicationResponse = DeletePlatformApplicationResponse'


instance NFData DeletePlatformApplicationResponse
         where
