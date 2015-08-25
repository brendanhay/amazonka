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
-- Module      : Network.AWS.CloudSearch.IndexDocuments
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tells the search domain to start indexing its documents using the latest
-- indexing options. This operation must be invoked to activate options
-- whose OptionStatus is 'RequiresIndexDocuments'.
--
-- /See:/ <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_IndexDocuments.html AWS API Reference> for IndexDocuments.
module Network.AWS.CloudSearch.IndexDocuments
    (
    -- * Creating a Request
      indexDocuments
    , IndexDocuments
    -- * Request Lenses
    , idDomainName

    -- * Destructuring the Response
    , indexDocumentsResponse
    , IndexDocumentsResponse
    -- * Response Lenses
    , idrsFieldNames
    , idrsStatus
    ) where

import           Network.AWS.CloudSearch.Types
import           Network.AWS.CloudSearch.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the 'IndexDocuments' operation.
-- Specifies the name of the domain you want to re-index.
--
-- /See:/ 'indexDocuments' smart constructor.
newtype IndexDocuments = IndexDocuments'
    { _idDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IndexDocuments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idDomainName'
indexDocuments
    :: Text -- ^ 'idDomainName'
    -> IndexDocuments
indexDocuments pDomainName_ =
    IndexDocuments'
    { _idDomainName = pDomainName_
    }

-- | Undocumented member.
idDomainName :: Lens' IndexDocuments Text
idDomainName = lens _idDomainName (\ s a -> s{_idDomainName = a});

instance AWSRequest IndexDocuments where
        type Rs IndexDocuments = IndexDocumentsResponse
        request = postQuery cloudSearch
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

-- | The result of an 'IndexDocuments' request. Contains the status of the
-- indexing operation, including the fields being indexed.
--
-- /See:/ 'indexDocumentsResponse' smart constructor.
data IndexDocumentsResponse = IndexDocumentsResponse'
    { _idrsFieldNames :: !(Maybe [Text])
    , _idrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'IndexDocumentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idrsFieldNames'
--
-- * 'idrsStatus'
indexDocumentsResponse
    :: Int -- ^ 'idrsStatus'
    -> IndexDocumentsResponse
indexDocumentsResponse pStatus_ =
    IndexDocumentsResponse'
    { _idrsFieldNames = Nothing
    , _idrsStatus = pStatus_
    }

-- | The names of the fields that are currently being indexed.
idrsFieldNames :: Lens' IndexDocumentsResponse [Text]
idrsFieldNames = lens _idrsFieldNames (\ s a -> s{_idrsFieldNames = a}) . _Default . _Coerce;

-- | The response status code.
idrsStatus :: Lens' IndexDocumentsResponse Int
idrsStatus = lens _idrsStatus (\ s a -> s{_idrsStatus = a});
