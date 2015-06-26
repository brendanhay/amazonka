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
    , idrStatusCode
    ) where

import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @IndexDocuments@ operation.
-- Specifies the name of the domain you want to re-index.
--
-- /See:/ 'indexDocuments' smart constructor.
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
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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

-- | The result of an @IndexDocuments@ request. Contains the status of the
-- indexing operation, including the fields being indexed.
--
-- /See:/ 'indexDocumentsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idrFieldNames'
--
-- * 'idrStatusCode'
data IndexDocumentsResponse = IndexDocumentsResponse'{_idrFieldNames :: Maybe [Text], _idrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'IndexDocumentsResponse' smart constructor.
indexDocumentsResponse :: Int -> IndexDocumentsResponse
indexDocumentsResponse pStatusCode = IndexDocumentsResponse'{_idrFieldNames = Nothing, _idrStatusCode = pStatusCode};

-- | The names of the fields that are currently being indexed.
idrFieldNames :: Lens' IndexDocumentsResponse [Text]
idrFieldNames = lens _idrFieldNames (\ s a -> s{_idrFieldNames = a}) . _Default;

-- | FIXME: Undocumented member.
idrStatusCode :: Lens' IndexDocumentsResponse Int
idrStatusCode = lens _idrStatusCode (\ s a -> s{_idrStatusCode = a});
