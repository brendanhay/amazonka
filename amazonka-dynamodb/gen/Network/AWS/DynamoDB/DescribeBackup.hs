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
-- Module      : Network.AWS.DynamoDB.DescribeBackup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing backup of a table.
--
--
-- You can call @DescribeBackup@ at a maximum rate of 10 times per second.
--
module Network.AWS.DynamoDB.DescribeBackup
    (
    -- * Creating a Request
      describeBackup
    , DescribeBackup
    -- * Request Lenses
    , dBackupARN

    -- * Destructuring the Response
    , describeBackupResponse
    , DescribeBackupResponse
    -- * Response Lenses
    , desrsBackupDescription
    , desrsResponseStatus
    ) where

import Network.AWS.DynamoDB.Types
import Network.AWS.DynamoDB.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBackup' smart constructor.
newtype DescribeBackup = DescribeBackup'
  { _dBackupARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBackup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dBackupARN' - The ARN associated with the backup.
describeBackup
    :: Text -- ^ 'dBackupARN'
    -> DescribeBackup
describeBackup pBackupARN_ = DescribeBackup' {_dBackupARN = pBackupARN_}


-- | The ARN associated with the backup.
dBackupARN :: Lens' DescribeBackup Text
dBackupARN = lens _dBackupARN (\ s a -> s{_dBackupARN = a})

instance AWSRequest DescribeBackup where
        type Rs DescribeBackup = DescribeBackupResponse
        request = postJSON dynamoDB
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBackupResponse' <$>
                   (x .?> "BackupDescription") <*> (pure (fromEnum s)))

instance Hashable DescribeBackup where

instance NFData DescribeBackup where

instance ToHeaders DescribeBackup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DynamoDB_20120810.DescribeBackup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON DescribeBackup where
        toJSON DescribeBackup'{..}
          = object
              (catMaybes [Just ("BackupArn" .= _dBackupARN)])

instance ToPath DescribeBackup where
        toPath = const "/"

instance ToQuery DescribeBackup where
        toQuery = const mempty

-- | /See:/ 'describeBackupResponse' smart constructor.
data DescribeBackupResponse = DescribeBackupResponse'
  { _desrsBackupDescription :: !(Maybe BackupDescription)
  , _desrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBackupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsBackupDescription' - Contains the description of the backup created for the table.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeBackupResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeBackupResponse
describeBackupResponse pResponseStatus_ =
  DescribeBackupResponse'
    {_desrsBackupDescription = Nothing, _desrsResponseStatus = pResponseStatus_}


-- | Contains the description of the backup created for the table.
desrsBackupDescription :: Lens' DescribeBackupResponse (Maybe BackupDescription)
desrsBackupDescription = lens _desrsBackupDescription (\ s a -> s{_desrsBackupDescription = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeBackupResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeBackupResponse where
