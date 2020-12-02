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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the suggesters configured for a domain. A suggester enables you to display possible matches before users finish typing their queries. Can be limited to specific suggesters by name. By default, shows all suggesters and includes any pending changes to the configuration. Set the @Deployed@ option to @true@ to show the active configuration and exclude pending changes. For more information, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions> in the /Amazon CloudSearch Developer Guide/ .
--
--
module Network.AWS.CloudSearch.DescribeSuggesters
    (
    -- * Creating a Request
      describeSuggesters
    , DescribeSuggesters
    -- * Request Lenses
    , dssDeployed
    , dssSuggesterNames
    , dssDomainName

    -- * Destructuring the Response
    , describeSuggestersResponse
    , DescribeSuggestersResponse
    -- * Response Lenses
    , dssrsResponseStatus
    , dssrsSuggesters
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.CloudSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DescribeSuggester' @ operation. Specifies the name of the domain you want to describe. To restrict the response to particular suggesters, specify the names of the suggesters you want to describe. To show the active configuration and exclude any pending changes, set the @Deployed@ option to @true@ .
--
--
--
-- /See:/ 'describeSuggesters' smart constructor.
data DescribeSuggesters = DescribeSuggesters'
  { _dssDeployed       :: !(Maybe Bool)
  , _dssSuggesterNames :: !(Maybe [Text])
  , _dssDomainName     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSuggesters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssDeployed' - Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
--
-- * 'dssSuggesterNames' - The suggesters you want to describe.
--
-- * 'dssDomainName' - The name of the domain you want to describe.
describeSuggesters
    :: Text -- ^ 'dssDomainName'
    -> DescribeSuggesters
describeSuggesters pDomainName_ =
  DescribeSuggesters'
    { _dssDeployed = Nothing
    , _dssSuggesterNames = Nothing
    , _dssDomainName = pDomainName_
    }


-- | Whether to display the deployed configuration (@true@ ) or include any pending changes (@false@ ). Defaults to @false@ .
dssDeployed :: Lens' DescribeSuggesters (Maybe Bool)
dssDeployed = lens _dssDeployed (\ s a -> s{_dssDeployed = a})

-- | The suggesters you want to describe.
dssSuggesterNames :: Lens' DescribeSuggesters [Text]
dssSuggesterNames = lens _dssSuggesterNames (\ s a -> s{_dssSuggesterNames = a}) . _Default . _Coerce

-- | The name of the domain you want to describe.
dssDomainName :: Lens' DescribeSuggesters Text
dssDomainName = lens _dssDomainName (\ s a -> s{_dssDomainName = a})

instance AWSRequest DescribeSuggesters where
        type Rs DescribeSuggesters =
             DescribeSuggestersResponse
        request = postQuery cloudSearch
        response
          = receiveXMLWrapper "DescribeSuggestersResult"
              (\ s h x ->
                 DescribeSuggestersResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "Suggesters" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable DescribeSuggesters where

instance NFData DescribeSuggesters where

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
--
--
-- /See:/ 'describeSuggestersResponse' smart constructor.
data DescribeSuggestersResponse = DescribeSuggestersResponse'
  { _dssrsResponseStatus :: !Int
  , _dssrsSuggesters     :: ![SuggesterStatus]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSuggestersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsResponseStatus' - -- | The response status code.
--
-- * 'dssrsSuggesters' - The suggesters configured for the domain specified in the request.
describeSuggestersResponse
    :: Int -- ^ 'dssrsResponseStatus'
    -> DescribeSuggestersResponse
describeSuggestersResponse pResponseStatus_ =
  DescribeSuggestersResponse'
    {_dssrsResponseStatus = pResponseStatus_, _dssrsSuggesters = mempty}


-- | -- | The response status code.
dssrsResponseStatus :: Lens' DescribeSuggestersResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\ s a -> s{_dssrsResponseStatus = a})

-- | The suggesters configured for the domain specified in the request.
dssrsSuggesters :: Lens' DescribeSuggestersResponse [SuggesterStatus]
dssrsSuggesters = lens _dssrsSuggesters (\ s a -> s{_dssrsSuggesters = a}) . _Coerce

instance NFData DescribeSuggestersResponse where
