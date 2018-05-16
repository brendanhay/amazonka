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
-- Module      : Network.AWS.Glue.ImportCatalogToGlue
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports an existing Athena Data Catalog to AWS Glue
--
--
module Network.AWS.Glue.ImportCatalogToGlue
    (
    -- * Creating a Request
      importCatalogToGlue
    , ImportCatalogToGlue
    -- * Request Lenses
    , ictgCatalogId

    -- * Destructuring the Response
    , importCatalogToGlueResponse
    , ImportCatalogToGlueResponse
    -- * Response Lenses
    , ictgrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'importCatalogToGlue' smart constructor.
newtype ImportCatalogToGlue = ImportCatalogToGlue'
  { _ictgCatalogId :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportCatalogToGlue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ictgCatalogId' - The ID of the catalog to import. Currently, this should be the AWS account ID.
importCatalogToGlue
    :: ImportCatalogToGlue
importCatalogToGlue = ImportCatalogToGlue' {_ictgCatalogId = Nothing}


-- | The ID of the catalog to import. Currently, this should be the AWS account ID.
ictgCatalogId :: Lens' ImportCatalogToGlue (Maybe Text)
ictgCatalogId = lens _ictgCatalogId (\ s a -> s{_ictgCatalogId = a})

instance AWSRequest ImportCatalogToGlue where
        type Rs ImportCatalogToGlue =
             ImportCatalogToGlueResponse
        request = postJSON glue
        response
          = receiveEmpty
              (\ s h x ->
                 ImportCatalogToGlueResponse' <$> (pure (fromEnum s)))

instance Hashable ImportCatalogToGlue where

instance NFData ImportCatalogToGlue where

instance ToHeaders ImportCatalogToGlue where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.ImportCatalogToGlue" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ImportCatalogToGlue where
        toJSON ImportCatalogToGlue'{..}
          = object
              (catMaybes [("CatalogId" .=) <$> _ictgCatalogId])

instance ToPath ImportCatalogToGlue where
        toPath = const "/"

instance ToQuery ImportCatalogToGlue where
        toQuery = const mempty

-- | /See:/ 'importCatalogToGlueResponse' smart constructor.
newtype ImportCatalogToGlueResponse = ImportCatalogToGlueResponse'
  { _ictgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ImportCatalogToGlueResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ictgrsResponseStatus' - -- | The response status code.
importCatalogToGlueResponse
    :: Int -- ^ 'ictgrsResponseStatus'
    -> ImportCatalogToGlueResponse
importCatalogToGlueResponse pResponseStatus_ =
  ImportCatalogToGlueResponse' {_ictgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ictgrsResponseStatus :: Lens' ImportCatalogToGlueResponse Int
ictgrsResponseStatus = lens _ictgrsResponseStatus (\ s a -> s{_ictgrsResponseStatus = a})

instance NFData ImportCatalogToGlueResponse where
