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
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified application. Amazon Kinesis Analytics halts application execution and deletes the application, including any application artifacts (such as in-application streams, reference table, and application code).
--
--
-- This operation requires permissions to perform the @kinesisanalytics:DeleteApplication@ action.
--
module Network.AWS.KinesisAnalytics.DeleteApplication
    (
    -- * Creating a Request
      deleteApplication
    , DeleteApplication
    -- * Request Lenses
    , dApplicationName
    , dCreateTimestamp

    -- * Destructuring the Response
    , deleteApplicationResponse
    , DeleteApplicationResponse
    -- * Response Lenses
    , drsResponseStatus
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteApplication' smart constructor.
data DeleteApplication = DeleteApplication'
  { _dApplicationName :: !Text
  , _dCreateTimestamp :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dApplicationName' - Name of the Amazon Kinesis Analytics application to delete.
--
-- * 'dCreateTimestamp' - You can use the @DescribeApplication@ operation to get this value.
deleteApplication
    :: Text -- ^ 'dApplicationName'
    -> UTCTime -- ^ 'dCreateTimestamp'
    -> DeleteApplication
deleteApplication pApplicationName_ pCreateTimestamp_ =
  DeleteApplication'
    { _dApplicationName = pApplicationName_
    , _dCreateTimestamp = _Time # pCreateTimestamp_
    }


-- | Name of the Amazon Kinesis Analytics application to delete.
dApplicationName :: Lens' DeleteApplication Text
dApplicationName = lens _dApplicationName (\ s a -> s{_dApplicationName = a})

-- | You can use the @DescribeApplication@ operation to get this value.
dCreateTimestamp :: Lens' DeleteApplication UTCTime
dCreateTimestamp = lens _dCreateTimestamp (\ s a -> s{_dCreateTimestamp = a}) . _Time

instance AWSRequest DeleteApplication where
        type Rs DeleteApplication = DeleteApplicationResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteApplicationResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteApplication where

instance NFData DeleteApplication where

instance ToHeaders DeleteApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.DeleteApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteApplication where
        toJSON DeleteApplication'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _dApplicationName),
                  Just ("CreateTimestamp" .= _dCreateTimestamp)])

instance ToPath DeleteApplication where
        toPath = const "/"

instance ToQuery DeleteApplication where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'deleteApplicationResponse' smart constructor.
newtype DeleteApplicationResponse = DeleteApplicationResponse'
  { _drsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteApplicationResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteApplicationResponse
deleteApplicationResponse pResponseStatus_ =
  DeleteApplicationResponse' {_drsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteApplicationResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteApplicationResponse where
