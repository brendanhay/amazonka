{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudSearch.IndexDocuments
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

-- | Tells the search domain to start indexing its documents using the latest
-- indexing options. This operation must be invoked to activate options
-- whose OptionStatus is @RequiresIndexDocuments@.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_IndexDocuments.html>
module Network.AWS.CloudSearch.IndexDocuments
    (
    -- * Request
      IndexDocuments
    -- ** Request constructor
    , indexDocuments
    -- ** Request lenses
    , idDomainName

    -- * Response
    , IndexDocumentsResponse
    -- ** Response constructor
    , indexDocumentsResponse
    -- ** Response lenses
    , idrFieldNames
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'indexDocuments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idDomainName'
newtype IndexDocuments = IndexDocuments'{_idDomainName :: Text} deriving (Eq, Read, Show)

-- | 'IndexDocuments' smart constructor.
indexDocuments :: Text -> IndexDocuments
indexDocuments pDomainName = IndexDocuments'{_idDomainName = pDomainName};

-- | FIXME: Undocumented member.
idDomainName :: Lens' IndexDocuments Text
idDomainName = lens _idDomainName (\ s a -> s{_idDomainName = a});

instance AWSRequest IndexDocuments where
        type Sv IndexDocuments = CloudSearch
        type Rs IndexDocuments = IndexDocumentsResponse
        request = post
        response
          = receiveXMLWrapper "IndexDocumentsResult"
              (\ s h x ->
                 IndexDocumentsResponse' <$>
                   (x .@? "FieldNames" .!@ mempty >>=
                      may (parseXMLList "member")))

instance ToHeaders IndexDocuments where
        toHeaders = const mempty

instance ToPath IndexDocuments where
        toPath = const "/"

instance ToQuery IndexDocuments where
        toQuery IndexDocuments'{..}
          = mconcat
              ["Action" =: ("IndexDocuments" :: ByteString),
               "Version" =: ("2013-01-01" :: ByteString),
               "DomainName" =: _idDomainName]

-- | /See:/ 'indexDocumentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idrFieldNames'
newtype IndexDocumentsResponse = IndexDocumentsResponse'{_idrFieldNames :: Maybe [Text]} deriving (Eq, Read, Show)

-- | 'IndexDocumentsResponse' smart constructor.
indexDocumentsResponse :: IndexDocumentsResponse
indexDocumentsResponse = IndexDocumentsResponse'{_idrFieldNames = Nothing};

-- | The names of the fields that are currently being indexed.
idrFieldNames :: Lens' IndexDocumentsResponse [Text]
idrFieldNames = lens _idrFieldNames (\ s a -> s{_idrFieldNames = a}) . _Default;
