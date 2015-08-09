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
-- Module      : Network.AWS.CloudSearch.DescribeSuggesters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeSuggesters.html AWS API Reference> for DescribeSuggesters.
module Network.AWS.CloudSearch.DescribeSuggesters
    (
    -- * Creating a Request
      DescribeSuggesters
    , describeSuggesters
    -- * Request Lenses
    , dssDeployed
    , dssSuggesterNames
    , dssDomainName

    -- * Destructuring the Response
    , DescribeSuggestersResponse
    , describeSuggestersResponse
    -- * Response Lenses
    , dssrsStatus
    , dssrsSuggesters
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

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
-- * 'dssDeployed'
--
-- * 'dssSuggesterNames'
--
-- * 'dssDomainName'
data DescribeSuggesters = DescribeSuggesters'
    { _dssDeployed :: !(Maybe Bool)
    , _dssSuggesterNames :: !(Maybe [Text])
    , _dssDomainName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSuggesters' smart constructor.
describeSuggesters :: Text -> DescribeSuggesters
describeSuggesters pDomainName_ = 
    DescribeSuggesters'
    { _dssDeployed = Nothing
    , _dssSuggesterNames = Nothing
    , _dssDomainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
dssDeployed :: Lens' DescribeSuggesters (Maybe Bool)
dssDeployed = lens _dssDeployed (\ s a -> s{_dssDeployed = a});

-- | The suggesters you want to describe.
dssSuggesterNames :: Lens' DescribeSuggesters [Text]
dssSuggesterNames = lens _dssSuggesterNames (\ s a -> s{_dssSuggesterNames = a}) . _Default . _Coerce;

-- | The name of the domain you want to describe.
dssDomainName :: Lens' DescribeSuggesters Text
dssDomainName = lens _dssDomainName (\ s a -> s{_dssDomainName = a});

instance AWSRequest DescribeSuggesters where
        type Sv DescribeSuggesters = CloudSearch
        type Rs DescribeSuggesters =
             DescribeSuggestersResponse
        request = postQuery
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
               "Deployed" =: _dssDeployed,
               "SuggesterNames" =:
                 toQuery
                   (toQueryList "member" <$> _dssSuggesterNames),
               "DomainName" =: _dssDomainName]

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
    { _dssrsStatus :: !Int
    , _dssrsSuggesters :: ![SuggesterStatus]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeSuggestersResponse' smart constructor.
describeSuggestersResponse :: Int -> DescribeSuggestersResponse
describeSuggestersResponse pStatus_ = 
    DescribeSuggestersResponse'
    { _dssrsStatus = pStatus_
    , _dssrsSuggesters = mempty
    }

-- | Undocumented member.
dssrsStatus :: Lens' DescribeSuggestersResponse Int
dssrsStatus = lens _dssrsStatus (\ s a -> s{_dssrsStatus = a});

-- | The suggesters configured for the domain specified in the request.
dssrsSuggesters :: Lens' DescribeSuggestersResponse [SuggesterStatus]
dssrsSuggesters = lens _dssrsSuggesters (\ s a -> s{_dssrsSuggesters = a}) . _Coerce;
