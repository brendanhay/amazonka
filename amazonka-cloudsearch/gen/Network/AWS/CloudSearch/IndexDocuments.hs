{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.IndexDocuments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Tells the search domain to start indexing its documents using the latest
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
    , idrStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the @IndexDocuments@ operation.
-- Specifies the name of the domain you want to re-index.
--
-- /See:/ 'indexDocuments' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'idDomainName'
newtype IndexDocuments = IndexDocuments'
    { _idDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'IndexDocuments' smart constructor.
indexDocuments :: Text -> IndexDocuments
indexDocuments pDomainName =
    IndexDocuments'
    { _idDomainName = pDomainName
    }

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
-- * 'idrStatus'
data IndexDocumentsResponse = IndexDocumentsResponse'
    { _idrFieldNames :: !(Maybe [Text])
    , _idrStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'IndexDocumentsResponse' smart constructor.
indexDocumentsResponse :: Int -> IndexDocumentsResponse
indexDocumentsResponse pStatus =
    IndexDocumentsResponse'
    { _idrFieldNames = Nothing
    , _idrStatus = pStatus
    }

-- | The names of the fields that are currently being indexed.
idrFieldNames :: Lens' IndexDocumentsResponse [Text]
idrFieldNames = lens _idrFieldNames (\ s a -> s{_idrFieldNames = a}) . _Default;

-- | FIXME: Undocumented member.
idrStatus :: Lens' IndexDocumentsResponse Int
idrStatus = lens _idrStatus (\ s a -> s{_idrStatus = a});
