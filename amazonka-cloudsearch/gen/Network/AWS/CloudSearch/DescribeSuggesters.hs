{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudSearch.DescribeSuggesters
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

-- | Gets the suggesters configured for a domain. A suggester enables you to
-- display possible matches before users finish typing their queries. Can
-- be limited to specific suggesters by name. By default, shows all
-- suggesters and includes any pending changes to the configuration. Set
-- the @Deployed@ option to @true@ to show the active configuration and
-- exclude pending changes. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeSuggesters.html>
module Network.AWS.CloudSearch.DescribeSuggesters
    (
    -- * Request
      DescribeSuggesters
    -- ** Request constructor
    , describeSuggesters
    -- ** Request lenses
    , desDeployed
    , desSuggesterNames
    , desDomainName

    -- * Response
    , DescribeSuggestersResponse
    -- ** Response constructor
    , describeSuggestersResponse
    -- ** Response lenses
    , dsr1Status
    , dsr1Suggesters
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @DescribeSuggester@ operation.
-- Specifies the name of the domain you want to describe. To restrict the
-- response to particular suggesters, specify the names of the suggesters
-- you want to describe. To show the active configuration and exclude any
-- pending changes, set the @Deployed@ option to @true@.
--
-- /See:/ 'describeSuggesters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desDeployed'
--
-- * 'desSuggesterNames'
--
-- * 'desDomainName'
data DescribeSuggesters = DescribeSuggesters'
    { _desDeployed       :: !(Maybe Bool)
    , _desSuggesterNames :: !(Maybe [Text])
    , _desDomainName     :: !Text
    } deriving (Eq,Read,Show)

-- | 'DescribeSuggesters' smart constructor.
describeSuggesters :: Text -> DescribeSuggesters
describeSuggesters pDomainName =
    DescribeSuggesters'
    { _desDeployed = Nothing
    , _desSuggesterNames = Nothing
    , _desDomainName = pDomainName
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
desDeployed :: Lens' DescribeSuggesters (Maybe Bool)
desDeployed = lens _desDeployed (\ s a -> s{_desDeployed = a});

-- | The suggesters you want to describe.
desSuggesterNames :: Lens' DescribeSuggesters [Text]
desSuggesterNames = lens _desSuggesterNames (\ s a -> s{_desSuggesterNames = a}) . _Default;

-- | The name of the domain you want to describe.
desDomainName :: Lens' DescribeSuggesters Text
desDomainName = lens _desDomainName (\ s a -> s{_desDomainName = a});

instance AWSRequest DescribeSuggesters where
        type Sv DescribeSuggesters = CloudSearch
        type Rs DescribeSuggesters =
             DescribeSuggestersResponse
        request = post
        response
          = receiveXMLWrapper "DescribeSuggestersResult"
              (\ s h x ->
                 DescribeSuggestersResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "Suggesters" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders DescribeSuggesters where
        toHeaders = const mempty

instance ToPath DescribeSuggesters where
        toPath = const "/"

instance ToQuery DescribeSuggesters where
        toQuery DescribeSuggesters'{..}
          = mconcat
              ["Action" =: ("DescribeSuggesters" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "Deployed" =: _desDeployed,
               "SuggesterNames" =:
                 toQuery
                   (toQueryList "member" <$> _desSuggesterNames),
               "DomainName" =: _desDomainName]

-- | The result of a @DescribeSuggesters@ request.
--
-- /See:/ 'describeSuggestersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsr1Status'
--
-- * 'dsr1Suggesters'
data DescribeSuggestersResponse = DescribeSuggestersResponse'
    { _dsr1Status     :: !Int
    , _dsr1Suggesters :: ![SuggesterStatus]
    } deriving (Eq,Read,Show)

-- | 'DescribeSuggestersResponse' smart constructor.
describeSuggestersResponse :: Int -> DescribeSuggestersResponse
describeSuggestersResponse pStatus =
    DescribeSuggestersResponse'
    { _dsr1Status = pStatus
    , _dsr1Suggesters = mempty
    }

-- | FIXME: Undocumented member.
dsr1Status :: Lens' DescribeSuggestersResponse Int
dsr1Status = lens _dsr1Status (\ s a -> s{_dsr1Status = a});

-- | The suggesters configured for the domain specified in the request.
dsr1Suggesters :: Lens' DescribeSuggestersResponse [SuggesterStatus]
dsr1Suggesters = lens _dsr1Suggesters (\ s a -> s{_dsr1Suggesters = a});
