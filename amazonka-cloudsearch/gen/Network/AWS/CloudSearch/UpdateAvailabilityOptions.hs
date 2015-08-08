{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.UpdateAvailabilityOptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the availability options for a domain. Enabling the Multi-AZ
-- option expands an Amazon CloudSearch domain to an additional
-- Availability Zone in the same Region to increase fault tolerance in the
-- event of a service disruption. Changes to the Multi-AZ option can take
-- about half an hour to become active. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html Configuring Availability Options>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_UpdateAvailabilityOptions.html AWS API Reference> for UpdateAvailabilityOptions.
module Network.AWS.CloudSearch.UpdateAvailabilityOptions
    (
    -- * Creating a Request
      UpdateAvailabilityOptions
    , updateAvailabilityOptions
    -- * Request Lenses
    , uaoDomainName
    , uaoMultiAZ

    -- * Destructuring the Response
    , UpdateAvailabilityOptionsResponse
    , updateAvailabilityOptionsResponse
    -- * Response Lenses
    , uaorsAvailabilityOptions
    , uaorsStatus
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
updateAvailabilityOptions pDomainName_ pMultiAZ_ =
    UpdateAvailabilityOptions'
    { _uaoDomainName = pDomainName_
    , _uaoMultiAZ = pMultiAZ_
    }

-- | Undocumented member.
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
        request = postQuery
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
-- * 'uaorsAvailabilityOptions'
--
-- * 'uaorsStatus'
data UpdateAvailabilityOptionsResponse = UpdateAvailabilityOptionsResponse'
    { _uaorsAvailabilityOptions :: !(Maybe AvailabilityOptionsStatus)
    , _uaorsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateAvailabilityOptionsResponse' smart constructor.
updateAvailabilityOptionsResponse :: Int -> UpdateAvailabilityOptionsResponse
updateAvailabilityOptionsResponse pStatus_ =
    UpdateAvailabilityOptionsResponse'
    { _uaorsAvailabilityOptions = Nothing
    , _uaorsStatus = pStatus_
    }

-- | The newly-configured availability options. Indicates whether Multi-AZ is
-- enabled for the domain.
uaorsAvailabilityOptions :: Lens' UpdateAvailabilityOptionsResponse (Maybe AvailabilityOptionsStatus)
uaorsAvailabilityOptions = lens _uaorsAvailabilityOptions (\ s a -> s{_uaorsAvailabilityOptions = a});

-- | Undocumented member.
uaorsStatus :: Lens' UpdateAvailabilityOptionsResponse Int
uaorsStatus = lens _uaorsStatus (\ s a -> s{_uaorsStatus = a});
