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
-- Module      : Network.AWS.QLDB.ExportJournalToS3
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.QLDB.ExportJournalToS3
    (
    -- * Creating a Request
      exportJournalToS3
    , ExportJournalToS3
    -- * Request Lenses
    , ejtsName
    , ejtsInclusiveStartTime
    , ejtsExclusiveEndTime
    , ejtsS3ExportConfiguration
    , ejtsRoleARN

    -- * Destructuring the Response
    , exportJournalToS3Response
    , ExportJournalToS3Response
    -- * Response Lenses
    , ejtsrsResponseStatus
    , ejtsrsExportId
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.QLDB.Types
import Network.AWS.QLDB.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'exportJournalToS3' smart constructor.
data ExportJournalToS3 = ExportJournalToS3'
  { _ejtsName                  :: !Text
  , _ejtsInclusiveStartTime    :: !POSIX
  , _ejtsExclusiveEndTime      :: !POSIX
  , _ejtsS3ExportConfiguration :: !S3ExportConfiguration
  , _ejtsRoleARN               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportJournalToS3' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ejtsName' - Undocumented member.
--
-- * 'ejtsInclusiveStartTime' - Undocumented member.
--
-- * 'ejtsExclusiveEndTime' - Undocumented member.
--
-- * 'ejtsS3ExportConfiguration' - Undocumented member.
--
-- * 'ejtsRoleARN' - Undocumented member.
exportJournalToS3
    :: Text -- ^ 'ejtsName'
    -> UTCTime -- ^ 'ejtsInclusiveStartTime'
    -> UTCTime -- ^ 'ejtsExclusiveEndTime'
    -> S3ExportConfiguration -- ^ 'ejtsS3ExportConfiguration'
    -> Text -- ^ 'ejtsRoleARN'
    -> ExportJournalToS3
exportJournalToS3 pName_ pInclusiveStartTime_ pExclusiveEndTime_ pS3ExportConfiguration_ pRoleARN_ =
  ExportJournalToS3'
    { _ejtsName = pName_
    , _ejtsInclusiveStartTime = _Time # pInclusiveStartTime_
    , _ejtsExclusiveEndTime = _Time # pExclusiveEndTime_
    , _ejtsS3ExportConfiguration = pS3ExportConfiguration_
    , _ejtsRoleARN = pRoleARN_
    }


-- | Undocumented member.
ejtsName :: Lens' ExportJournalToS3 Text
ejtsName = lens _ejtsName (\ s a -> s{_ejtsName = a})

-- | Undocumented member.
ejtsInclusiveStartTime :: Lens' ExportJournalToS3 UTCTime
ejtsInclusiveStartTime = lens _ejtsInclusiveStartTime (\ s a -> s{_ejtsInclusiveStartTime = a}) . _Time

-- | Undocumented member.
ejtsExclusiveEndTime :: Lens' ExportJournalToS3 UTCTime
ejtsExclusiveEndTime = lens _ejtsExclusiveEndTime (\ s a -> s{_ejtsExclusiveEndTime = a}) . _Time

-- | Undocumented member.
ejtsS3ExportConfiguration :: Lens' ExportJournalToS3 S3ExportConfiguration
ejtsS3ExportConfiguration = lens _ejtsS3ExportConfiguration (\ s a -> s{_ejtsS3ExportConfiguration = a})

-- | Undocumented member.
ejtsRoleARN :: Lens' ExportJournalToS3 Text
ejtsRoleARN = lens _ejtsRoleARN (\ s a -> s{_ejtsRoleARN = a})

instance AWSRequest ExportJournalToS3 where
        type Rs ExportJournalToS3 = ExportJournalToS3Response
        request = postJSON qldb
        response
          = receiveJSON
              (\ s h x ->
                 ExportJournalToS3Response' <$>
                   (pure (fromEnum s)) <*> (x .:> "ExportId"))

instance Hashable ExportJournalToS3 where

instance NFData ExportJournalToS3 where

instance ToHeaders ExportJournalToS3 where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ExportJournalToS3 where
        toJSON ExportJournalToS3'{..}
          = object
              (catMaybes
                 [Just
                    ("InclusiveStartTime" .= _ejtsInclusiveStartTime),
                  Just ("ExclusiveEndTime" .= _ejtsExclusiveEndTime),
                  Just
                    ("S3ExportConfiguration" .=
                       _ejtsS3ExportConfiguration),
                  Just ("RoleArn" .= _ejtsRoleARN)])

instance ToPath ExportJournalToS3 where
        toPath ExportJournalToS3'{..}
          = mconcat
              ["/ledgers/", toBS _ejtsName, "/journal-s3-exports"]

instance ToQuery ExportJournalToS3 where
        toQuery = const mempty

-- | /See:/ 'exportJournalToS3Response' smart constructor.
data ExportJournalToS3Response = ExportJournalToS3Response'
  { _ejtsrsResponseStatus :: !Int
  , _ejtsrsExportId       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExportJournalToS3Response' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ejtsrsResponseStatus' - -- | The response status code.
--
-- * 'ejtsrsExportId' - Undocumented member.
exportJournalToS3Response
    :: Int -- ^ 'ejtsrsResponseStatus'
    -> Text -- ^ 'ejtsrsExportId'
    -> ExportJournalToS3Response
exportJournalToS3Response pResponseStatus_ pExportId_ =
  ExportJournalToS3Response'
    {_ejtsrsResponseStatus = pResponseStatus_, _ejtsrsExportId = pExportId_}


-- | -- | The response status code.
ejtsrsResponseStatus :: Lens' ExportJournalToS3Response Int
ejtsrsResponseStatus = lens _ejtsrsResponseStatus (\ s a -> s{_ejtsrsResponseStatus = a})

-- | Undocumented member.
ejtsrsExportId :: Lens' ExportJournalToS3Response Text
ejtsrsExportId = lens _ejtsrsExportId (\ s a -> s{_ejtsrsExportId = a})

instance NFData ExportJournalToS3Response where
