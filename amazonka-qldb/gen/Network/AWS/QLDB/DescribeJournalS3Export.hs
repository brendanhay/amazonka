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
-- Module      : Network.AWS.QLDB.DescribeJournalS3Export
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.QLDB.DescribeJournalS3Export
    (
    -- * Creating a Request
      describeJournalS3Export
    , DescribeJournalS3Export
    -- * Request Lenses
    , djseName
    , djseExportId

    -- * Destructuring the Response
    , describeJournalS3ExportResponse
    , DescribeJournalS3ExportResponse
    -- * Response Lenses
    , djsersResponseStatus
    , djsersExportDescription
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeJournalS3Export' smart constructor.
data DescribeJournalS3Export = DescribeJournalS3Export'
  { _djseName     :: !Text
  , _djseExportId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJournalS3Export' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djseName' - Undocumented member.
--
-- * 'djseExportId' - Undocumented member.
describeJournalS3Export
    :: Text -- ^ 'djseName'
    -> Text -- ^ 'djseExportId'
    -> DescribeJournalS3Export
describeJournalS3Export pName_ pExportId_ =
  DescribeJournalS3Export' {_djseName = pName_, _djseExportId = pExportId_}


-- | Undocumented member.
djseName :: Lens' DescribeJournalS3Export Text
djseName = lens _djseName (\ s a -> s{_djseName = a})

-- | Undocumented member.
djseExportId :: Lens' DescribeJournalS3Export Text
djseExportId = lens _djseExportId (\ s a -> s{_djseExportId = a})

instance AWSRequest DescribeJournalS3Export where
        type Rs DescribeJournalS3Export =
             DescribeJournalS3ExportResponse
        request = get qldb
        response
          = receiveJSON
              (\ s h x ->
                 DescribeJournalS3ExportResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "ExportDescription"))

instance Hashable DescribeJournalS3Export where

instance NFData DescribeJournalS3Export where

instance ToHeaders DescribeJournalS3Export where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToPath DescribeJournalS3Export where
        toPath DescribeJournalS3Export'{..}
          = mconcat
              ["/ledgers/", toBS _djseName, "/journal-s3-exports/",
               toBS _djseExportId]

instance ToQuery DescribeJournalS3Export where
        toQuery = const mempty

-- | /See:/ 'describeJournalS3ExportResponse' smart constructor.
data DescribeJournalS3ExportResponse = DescribeJournalS3ExportResponse'
  { _djsersResponseStatus    :: !Int
  , _djsersExportDescription :: !JournalS3ExportDescription
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJournalS3ExportResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djsersResponseStatus' - -- | The response status code.
--
-- * 'djsersExportDescription' - Undocumented member.
describeJournalS3ExportResponse
    :: Int -- ^ 'djsersResponseStatus'
    -> JournalS3ExportDescription -- ^ 'djsersExportDescription'
    -> DescribeJournalS3ExportResponse
describeJournalS3ExportResponse pResponseStatus_ pExportDescription_ =
  DescribeJournalS3ExportResponse'
    { _djsersResponseStatus = pResponseStatus_
    , _djsersExportDescription = pExportDescription_
    }


-- | -- | The response status code.
djsersResponseStatus :: Lens' DescribeJournalS3ExportResponse Int
djsersResponseStatus = lens _djsersResponseStatus (\ s a -> s{_djsersResponseStatus = a})

-- | Undocumented member.
djsersExportDescription :: Lens' DescribeJournalS3ExportResponse JournalS3ExportDescription
djsersExportDescription = lens _djsersExportDescription (\ s a -> s{_djsersExportDescription = a})

instance NFData DescribeJournalS3ExportResponse where
