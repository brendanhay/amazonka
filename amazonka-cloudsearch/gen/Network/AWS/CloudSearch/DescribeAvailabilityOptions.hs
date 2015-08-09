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
-- Module      : Network.AWS.CloudSearch.DescribeAvailabilityOptions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the availability options configured for a domain. By default, shows
-- the configuration with any pending changes. Set the @Deployed@ option to
-- @true@ to show the active configuration and exclude pending changes. For
-- more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-availability-options.html Configuring Availability Options>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeAvailabilityOptions.html AWS API Reference> for DescribeAvailabilityOptions.
module Network.AWS.CloudSearch.DescribeAvailabilityOptions
    (
    -- * Creating a Request
      DescribeAvailabilityOptions
    , describeAvailabilityOptions
    -- * Request Lenses
    , daoDeployed
    , daoDomainName

    -- * Destructuring the Response
    , DescribeAvailabilityOptionsResponse
    , describeAvailabilityOptionsResponse
    -- * Response Lenses
    , daorsAvailabilityOptions
    , daorsStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.CloudSearch.Types.Product
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAvailabilityOptions' smart constructor.
describeAvailabilityOptions :: Text -> DescribeAvailabilityOptions
describeAvailabilityOptions pDomainName_ =
    DescribeAvailabilityOptions'
    { _daoDeployed = Nothing
    , _daoDomainName = pDomainName_
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
        request = postQuery
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
-- * 'daorsAvailabilityOptions'
--
-- * 'daorsStatus'
data DescribeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse'
    { _daorsAvailabilityOptions :: !(Maybe AvailabilityOptionsStatus)
    , _daorsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAvailabilityOptionsResponse' smart constructor.
describeAvailabilityOptionsResponse :: Int -> DescribeAvailabilityOptionsResponse
describeAvailabilityOptionsResponse pStatus_ =
    DescribeAvailabilityOptionsResponse'
    { _daorsAvailabilityOptions = Nothing
    , _daorsStatus = pStatus_
    }

-- | The availability options configured for the domain. Indicates whether
-- Multi-AZ is enabled for the domain.
daorsAvailabilityOptions :: Lens' DescribeAvailabilityOptionsResponse (Maybe AvailabilityOptionsStatus)
daorsAvailabilityOptions = lens _daorsAvailabilityOptions (\ s a -> s{_daorsAvailabilityOptions = a});

-- | Undocumented member.
daorsStatus :: Lens' DescribeAvailabilityOptionsResponse Int
daorsStatus = lens _daorsStatus (\ s a -> s{_daorsStatus = a});
