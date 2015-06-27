{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.DescribeAvailabilityOptions
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

-- | Gets the availability options configured for a domain. By default, shows
-- the configuration with any pending changes. Set the @Deployed@ option to
-- @true@ to show the active configuration and exclude pending changes. For
-- more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html Configuring Availability Options>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeAvailabilityOptions.html>
module Network.AWS.CloudSearch.DescribeAvailabilityOptions
    (
    -- * Request
      DescribeAvailabilityOptions
    -- ** Request constructor
    , describeAvailabilityOptions
    -- ** Request lenses
    , daoDeployed
    , daoDomainName

    -- * Response
    , DescribeAvailabilityOptionsResponse
    -- ** Response constructor
    , describeAvailabilityOptionsResponse
    -- ** Response lenses
    , daorAvailabilityOptions
    , daorStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DescribeAvailabilityOptions@
-- operation. Specifies the name of the domain you want to describe. To
-- show the active configuration and exclude any pending changes, set the
-- Deployed option to @true@.
--
-- /See:/ 'describeAvailabilityOptions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daoDeployed'
--
-- * 'daoDomainName'
data DescribeAvailabilityOptions = DescribeAvailabilityOptions'
    { _daoDeployed   :: !(Maybe Bool)
    , _daoDomainName :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeAvailabilityOptions' smart constructor.
describeAvailabilityOptions :: Text -> DescribeAvailabilityOptions
describeAvailabilityOptions pDomainName =
    DescribeAvailabilityOptions'
    { _daoDeployed = Nothing
    , _daoDomainName = pDomainName
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
daoDeployed :: Lens' DescribeAvailabilityOptions (Maybe Bool)
daoDeployed = lens _daoDeployed (\ s a -> s{_daoDeployed = a});

-- | The name of the domain you want to describe.
daoDomainName :: Lens' DescribeAvailabilityOptions Text
daoDomainName = lens _daoDomainName (\ s a -> s{_daoDomainName = a});

instance AWSRequest DescribeAvailabilityOptions where
        type Sv DescribeAvailabilityOptions = CloudSearch
        type Rs DescribeAvailabilityOptions =
             DescribeAvailabilityOptionsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeAvailabilityOptionsResult"
              (\ s h x ->
                 DescribeAvailabilityOptionsResponse' <$>
                   (x .@? "AvailabilityOptions") <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeAvailabilityOptions where
        toHeaders = const mempty

instance ToPath DescribeAvailabilityOptions where
        toPath = const "/"

instance ToQuery DescribeAvailabilityOptions where
        toQuery DescribeAvailabilityOptions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeAvailabilityOptions" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "Deployed" =: _daoDeployed,
               "DomainName" =: _daoDomainName]

-- | The result of a @DescribeAvailabilityOptions@ request. Indicates whether
-- or not the Multi-AZ option is enabled for the domain specified in the
-- request.
--
-- /See:/ 'describeAvailabilityOptionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daorAvailabilityOptions'
--
-- * 'daorStatus'
data DescribeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse'
    { _daorAvailabilityOptions :: !(Maybe AvailabilityOptionsStatus)
    , _daorStatus              :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeAvailabilityOptionsResponse' smart constructor.
describeAvailabilityOptionsResponse :: Int -> DescribeAvailabilityOptionsResponse
describeAvailabilityOptionsResponse pStatus =
    DescribeAvailabilityOptionsResponse'
    { _daorAvailabilityOptions = Nothing
    , _daorStatus = pStatus
    }

-- | The availability options configured for the domain. Indicates whether
-- Multi-AZ is enabled for the domain.
daorAvailabilityOptions :: Lens' DescribeAvailabilityOptionsResponse (Maybe AvailabilityOptionsStatus)
daorAvailabilityOptions = lens _daorAvailabilityOptions (\ s a -> s{_daorAvailabilityOptions = a});

-- | FIXME: Undocumented member.
daorStatus :: Lens' DescribeAvailabilityOptionsResponse Int
daorStatus = lens _daorStatus (\ s a -> s{_daorStatus = a});
