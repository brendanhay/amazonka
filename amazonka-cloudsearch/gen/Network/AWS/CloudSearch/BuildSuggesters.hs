{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudSearch.BuildSuggesters
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

-- | Indexes the search suggestions. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html#configuring-suggesters Configuring Suggesters>
-- in the /Amazon CloudSearch Developer Guide/.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_BuildSuggesters.html>
module Network.AWS.CloudSearch.BuildSuggesters
    (
    -- * Request
      BuildSuggesters
    -- ** Request constructor
    , buildSuggesters
    -- ** Request lenses
    , bsDomainName

    -- * Response
    , BuildSuggestersResponse
    -- ** Response constructor
    , buildSuggestersResponse
    -- ** Response lenses
    , bsrFieldNames
    , bsrStatusCode
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @BuildSuggester@ operation.
-- Specifies the name of the domain you want to update.
--
-- /See:/ 'buildSuggesters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bsDomainName'
newtype BuildSuggesters = BuildSuggesters'{_bsDomainName :: Text} deriving (Eq, Read, Show)

-- | 'BuildSuggesters' smart constructor.
buildSuggesters :: Text -> BuildSuggesters
buildSuggesters pDomainName = BuildSuggesters'{_bsDomainName = pDomainName};

-- | FIXME: Undocumented member.
bsDomainName :: Lens' BuildSuggesters Text
bsDomainName = lens _bsDomainName (\ s a -> s{_bsDomainName = a});

instance AWSRequest BuildSuggesters where
        type Sv BuildSuggesters = CloudSearch
        type Rs BuildSuggesters = BuildSuggestersResponse
        request = post
        response
          = receiveXMLWrapper "BuildSuggestersResult"
              (\ s h x ->
                 BuildSuggestersResponse' <$>
                   (x .@? "FieldNames" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance ToHeaders BuildSuggesters where
        toHeaders = const mempty

instance ToPath BuildSuggesters where
        toPath = const "/"

instance ToQuery BuildSuggesters where
        toQuery BuildSuggesters'{..}
          = mconcat
              ["Action" =: ("BuildSuggesters" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _bsDomainName]

-- | The result of a @BuildSuggester@ request. Contains a list of the fields
-- used for suggestions.
--
-- /See:/ 'buildSuggestersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bsrFieldNames'
--
-- * 'bsrStatusCode'
data BuildSuggestersResponse = BuildSuggestersResponse'{_bsrFieldNames :: Maybe [Text], _bsrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'BuildSuggestersResponse' smart constructor.
buildSuggestersResponse :: Int -> BuildSuggestersResponse
buildSuggestersResponse pStatusCode = BuildSuggestersResponse'{_bsrFieldNames = Nothing, _bsrStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
bsrFieldNames :: Lens' BuildSuggestersResponse [Text]
bsrFieldNames = lens _bsrFieldNames (\ s a -> s{_bsrFieldNames = a}) . _Default;

-- | FIXME: Undocumented member.
bsrStatusCode :: Lens' BuildSuggestersResponse Int
bsrStatusCode = lens _bsrStatusCode (\ s a -> s{_bsrStatusCode = a});
