{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CloudSearch.UpdateAvailabilityOptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Configures the availability options for a domain. Enabling the Multi-AZ
-- option expands an Amazon CloudSearch domain to an additional
-- Availability Zone in the same Region to increase fault tolerance in the
-- event of a service disruption. Changes to the Multi-AZ option can take
-- about half an hour to become active. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html Configuring Availability Options>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_UpdateAvailabilityOptions.html>
module Network.AWS.CloudSearch.UpdateAvailabilityOptions
    (
    -- * Request
      UpdateAvailabilityOptions
    -- ** Request constructor
    , updateAvailabilityOptions
    -- ** Request lenses
    , uaoDomainName
    , uaoMultiAZ

    -- * Response
    , UpdateAvailabilityOptionsResponse
    -- ** Response constructor
    , updateAvailabilityOptionsResponse
    -- ** Response lenses
    , uaorAvailabilityOptions
    , uaorStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @UpdateAvailabilityOptions@
-- operation. Specifies the name of the domain you want to update and the
-- Multi-AZ availability option.
--
-- /See:/ 'updateAvailabilityOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaoDomainName'
--
-- * 'uaoMultiAZ'
data UpdateAvailabilityOptions = UpdateAvailabilityOptions'
    { _uaoDomainName :: !Text
    , _uaoMultiAZ    :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAvailabilityOptions' smart constructor.
updateAvailabilityOptions :: Text -> Bool -> UpdateAvailabilityOptions
updateAvailabilityOptions pDomainName pMultiAZ =
    UpdateAvailabilityOptions'
    { _uaoDomainName = pDomainName
    , _uaoMultiAZ = pMultiAZ
    }

-- | FIXME: Undocumented member.
uaoDomainName :: Lens' UpdateAvailabilityOptions Text
uaoDomainName = lens _uaoDomainName (\ s a -> s{_uaoDomainName = a});

-- | You expand an existing search domain to a second Availability Zone by
-- setting the Multi-AZ option to true. Similarly, you can turn off the
-- Multi-AZ option to downgrade the domain to a single Availability Zone by
-- setting the Multi-AZ option to @false@.
uaoMultiAZ :: Lens' UpdateAvailabilityOptions Bool
uaoMultiAZ = lens _uaoMultiAZ (\ s a -> s{_uaoMultiAZ = a});

instance AWSRequest UpdateAvailabilityOptions where
        type Sv UpdateAvailabilityOptions = CloudSearch
        type Rs UpdateAvailabilityOptions =
             UpdateAvailabilityOptionsResponse
        request = post
        response
          = receiveXMLWrapper "UpdateAvailabilityOptionsResult"
              (\ s h x ->
                 UpdateAvailabilityOptionsResponse' <$>
                   (x .@? "AvailabilityOptions") <*>
                     (pure (fromEnum s)))

instance ToHeaders UpdateAvailabilityOptions where
        toHeaders = const mempty

instance ToPath UpdateAvailabilityOptions where
        toPath = const "/"

instance ToQuery UpdateAvailabilityOptions where
        toQuery UpdateAvailabilityOptions'{..}
          = mconcat
              ["Action" =:
                 ("UpdateAvailabilityOptions" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _uaoDomainName,
               "MultiAZ" =: _uaoMultiAZ]

-- | The result of a @UpdateAvailabilityOptions@ request. Contains the status
-- of the domain\'s availability options.
--
-- /See:/ 'updateAvailabilityOptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uaorAvailabilityOptions'
--
-- * 'uaorStatus'
data UpdateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse'
    { _uaorAvailabilityOptions :: !(Maybe AvailabilityOptionsStatus)
    , _uaorStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAvailabilityOptionsResponse' smart constructor.
updateAvailabilityOptionsResponse :: Int -> UpdateAvailabilityOptionsResponse
updateAvailabilityOptionsResponse pStatus =
    UpdateAvailabilityOptionsResponse'
    { _uaorAvailabilityOptions = Nothing
    , _uaorStatus = pStatus
    }

-- | The newly-configured availability options. Indicates whether Multi-AZ is
-- enabled for the domain.
uaorAvailabilityOptions :: Lens' UpdateAvailabilityOptionsResponse (Maybe AvailabilityOptionsStatus)
uaorAvailabilityOptions = lens _uaorAvailabilityOptions (\ s a -> s{_uaorAvailabilityOptions = a});

-- | FIXME: Undocumented member.
uaorStatus :: Lens' UpdateAvailabilityOptionsResponse Int
uaorStatus = lens _uaorStatus (\ s a -> s{_uaorStatus = a});
