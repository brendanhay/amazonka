{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SES.DeleteVerifiedEmailAddress
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

-- | Deletes the specified email address from the list of verified addresses.
--
-- The DeleteVerifiedEmailAddress action is deprecated as of the May 15,
-- 2012 release of Domain Verification. The DeleteIdentity action is now
-- preferred.
--
-- This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_DeleteVerifiedEmailAddress.html>
module Network.AWS.SES.DeleteVerifiedEmailAddress
    (
    -- * Request
      DeleteVerifiedEmailAddress
    -- ** Request constructor
    , deleteVerifiedEmailAddress
    -- ** Request lenses
    , dveaEmailAddress

    -- * Response
    , DeleteVerifiedEmailAddressResponse
    -- ** Response constructor
    , deleteVerifiedEmailAddressResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types

-- | Represents a request instructing the service to delete an address from
-- the list of verified email addresses.
--
-- /See:/ 'deleteVerifiedEmailAddress' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dveaEmailAddress'
newtype DeleteVerifiedEmailAddress = DeleteVerifiedEmailAddress'
    { _dveaEmailAddress :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteVerifiedEmailAddress' smart constructor.
deleteVerifiedEmailAddress :: Text -> DeleteVerifiedEmailAddress
deleteVerifiedEmailAddress pEmailAddress =
    DeleteVerifiedEmailAddress'
    { _dveaEmailAddress = pEmailAddress
    }

-- | An email address to be removed from the list of verified addresses.
dveaEmailAddress :: Lens' DeleteVerifiedEmailAddress Text
dveaEmailAddress = lens _dveaEmailAddress (\ s a -> s{_dveaEmailAddress = a});

instance AWSRequest DeleteVerifiedEmailAddress where
        type Sv DeleteVerifiedEmailAddress = SES
        type Rs DeleteVerifiedEmailAddress =
             DeleteVerifiedEmailAddressResponse
        request = post
        response
          = receiveNull DeleteVerifiedEmailAddressResponse'

instance ToHeaders DeleteVerifiedEmailAddress where
        toHeaders = const mempty

instance ToPath DeleteVerifiedEmailAddress where
        toPath = const "/"

instance ToQuery DeleteVerifiedEmailAddress where
        toQuery DeleteVerifiedEmailAddress'{..}
          = mconcat
              ["Action" =:
                 ("DeleteVerifiedEmailAddress" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EmailAddress" =: _dveaEmailAddress]

-- | /See:/ 'deleteVerifiedEmailAddressResponse' smart constructor.
data DeleteVerifiedEmailAddressResponse =
    DeleteVerifiedEmailAddressResponse'
    deriving (Eq,Read,Show)

-- | 'DeleteVerifiedEmailAddressResponse' smart constructor.
deleteVerifiedEmailAddressResponse :: DeleteVerifiedEmailAddressResponse
deleteVerifiedEmailAddressResponse = DeleteVerifiedEmailAddressResponse'
