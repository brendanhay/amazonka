{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.DescribeSuggesters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets the suggesters configured for a domain. A suggester enables you to
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
    , dsrqDeployed
    , dsrqSuggesterNames
    , dsrqDomainName

    -- * Response
    , DescribeSuggestersResponse
    -- ** Response constructor
    , describeSuggestersResponse
    -- ** Response lenses
    , dssrsStatus
    , dssrsSuggesters
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
-- * 'dsrqDeployed'
--
-- * 'dsrqSuggesterNames'
--
-- * 'dsrqDomainName'
data DescribeSuggesters = DescribeSuggesters'
    { _dsrqDeployed       :: !(Maybe Bool)
    , _dsrqSuggesterNames :: !(Maybe [Text])
    , _dsrqDomainName     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSuggesters' smart constructor.
describeSuggesters :: Text -> DescribeSuggesters
describeSuggesters pDomainName =
    DescribeSuggesters'
    { _dsrqDeployed = Nothing
    , _dsrqSuggesterNames = Nothing
    , _dsrqDomainName = pDomainName
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
dsrqDeployed :: Lens' DescribeSuggesters (Maybe Bool)
dsrqDeployed = lens _dsrqDeployed (\ s a -> s{_dsrqDeployed = a});

-- | The suggesters you want to describe.
dsrqSuggesterNames :: Lens' DescribeSuggesters [Text]
dsrqSuggesterNames = lens _dsrqSuggesterNames (\ s a -> s{_dsrqSuggesterNames = a}) . _Default;

-- | The name of the domain you want to describe.
dsrqDomainName :: Lens' DescribeSuggesters Text
dsrqDomainName = lens _dsrqDomainName (\ s a -> s{_dsrqDomainName = a});

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
               "Deployed" =: _dsrqDeployed,
               "SuggesterNames" =:
                 toQuery
                   (toQueryList "member" <$> _dsrqSuggesterNames),
               "DomainName" =: _dsrqDomainName]

-- | The result of a @DescribeSuggesters@ request.
--
-- /See:/ 'describeSuggestersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dssrsStatus'
--
-- * 'dssrsSuggesters'
data DescribeSuggestersResponse = DescribeSuggestersResponse'
    { _dssrsStatus     :: !Int
    , _dssrsSuggesters :: ![SuggesterStatus]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSuggestersResponse' smart constructor.
describeSuggestersResponse :: Int -> DescribeSuggestersResponse
describeSuggestersResponse pStatus =
    DescribeSuggestersResponse'
    { _dssrsStatus = pStatus
    , _dssrsSuggesters = mempty
    }

-- | FIXME: Undocumented member.
dssrsStatus :: Lens' DescribeSuggestersResponse Int
dssrsStatus = lens _dssrsStatus (\ s a -> s{_dssrsStatus = a});

-- | The suggesters configured for the domain specified in the request.
dssrsSuggesters :: Lens' DescribeSuggestersResponse [SuggesterStatus]
dssrsSuggesters = lens _dssrsSuggesters (\ s a -> s{_dssrsSuggesters = a});
