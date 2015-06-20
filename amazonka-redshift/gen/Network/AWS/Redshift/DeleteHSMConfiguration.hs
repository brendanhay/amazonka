{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Redshift.DeleteHSMConfiguration
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

-- | Deletes the specified Amazon Redshift HSM configuration.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DeleteHSMConfiguration.html>
module Network.AWS.Redshift.DeleteHSMConfiguration
    (
    -- * Request
      DeleteHSMConfiguration
    -- ** Request constructor
    , deleteHSMConfiguration
    -- ** Request lenses
    , dhcHSMConfigurationIdentifier

    -- * Response
    , DeleteHSMConfigurationResponse
    -- ** Response constructor
    , deleteHSMConfigurationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteHSMConfiguration' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dhcHSMConfigurationIdentifier'
newtype DeleteHSMConfiguration = DeleteHSMConfiguration'{_dhcHSMConfigurationIdentifier :: Text} deriving (Eq, Read, Show)

-- | 'DeleteHSMConfiguration' smart constructor.
deleteHSMConfiguration :: Text -> DeleteHSMConfiguration
deleteHSMConfiguration pHSMConfigurationIdentifier = DeleteHSMConfiguration'{_dhcHSMConfigurationIdentifier = pHSMConfigurationIdentifier};

-- | The identifier of the Amazon Redshift HSM configuration to be deleted.
dhcHSMConfigurationIdentifier :: Lens' DeleteHSMConfiguration Text
dhcHSMConfigurationIdentifier = lens _dhcHSMConfigurationIdentifier (\ s a -> s{_dhcHSMConfigurationIdentifier = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteHSMConfiguration where
        type Sv DeleteHSMConfiguration = Redshift
        type Rs DeleteHSMConfiguration =
             DeleteHSMConfigurationResponse
        request = post
        response
          = receiveNull DeleteHSMConfigurationResponse'

instance ToHeaders DeleteHSMConfiguration where
        toHeaders = const mempty

instance ToPath DeleteHSMConfiguration where
        toPath = const "/"

instance ToQuery DeleteHSMConfiguration where
        toQuery DeleteHSMConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("DeleteHSMConfiguration" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "HsmConfigurationIdentifier" =:
                 _dhcHSMConfigurationIdentifier]

-- | /See:/ 'deleteHSMConfigurationResponse' smart constructor.
data DeleteHSMConfigurationResponse = DeleteHSMConfigurationResponse' deriving (Eq, Read, Show)

-- | 'DeleteHSMConfigurationResponse' smart constructor.
deleteHSMConfigurationResponse :: DeleteHSMConfigurationResponse
deleteHSMConfigurationResponse = DeleteHSMConfigurationResponse';
