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
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationOutput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes output destination configuration from your application configuration. Amazon Kinesis Analytics will no longer write data from the corresponding in-application stream to the external output destination.
--
--
-- This operation requires permissions to perform the @kinesisanalytics:DeleteApplicationOutput@ action.
--
module Network.AWS.KinesisAnalytics.DeleteApplicationOutput
    (
    -- * Creating a Request
      deleteApplicationOutput
    , DeleteApplicationOutput
    -- * Request Lenses
    , daoApplicationName
    , daoCurrentApplicationVersionId
    , daoOutputId

    -- * Destructuring the Response
    , deleteApplicationOutputResponse
    , DeleteApplicationOutputResponse
    -- * Response Lenses
    , daorsResponseStatus
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
-- /See:/ 'deleteApplicationOutput' smart constructor.
data DeleteApplicationOutput = DeleteApplicationOutput'
  { _daoApplicationName             :: !Text
  , _daoCurrentApplicationVersionId :: !Nat
  , _daoOutputId                    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daoApplicationName' - Amazon Kinesis Analytics application name.
--
-- * 'daoCurrentApplicationVersionId' - Amazon Kinesis Analytics application version. You can use the 'DescribeApplication' operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- * 'daoOutputId' - The ID of the configuration to delete. Each output configuration that is added to the application, either when the application is created or later using the 'AddApplicationOutput' operation, has a unique ID. You need to provide the ID to uniquely identify the output configuration that you want to delete from the application configuration. You can use the 'DescribeApplication' operation to get the specific @OutputId@ .
deleteApplicationOutput
    :: Text -- ^ 'daoApplicationName'
    -> Natural -- ^ 'daoCurrentApplicationVersionId'
    -> Text -- ^ 'daoOutputId'
    -> DeleteApplicationOutput
deleteApplicationOutput pApplicationName_ pCurrentApplicationVersionId_ pOutputId_ =
  DeleteApplicationOutput'
    { _daoApplicationName = pApplicationName_
    , _daoCurrentApplicationVersionId = _Nat # pCurrentApplicationVersionId_
    , _daoOutputId = pOutputId_
    }


-- | Amazon Kinesis Analytics application name.
daoApplicationName :: Lens' DeleteApplicationOutput Text
daoApplicationName = lens _daoApplicationName (\ s a -> s{_daoApplicationName = a})

-- | Amazon Kinesis Analytics application version. You can use the 'DescribeApplication' operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
daoCurrentApplicationVersionId :: Lens' DeleteApplicationOutput Natural
daoCurrentApplicationVersionId = lens _daoCurrentApplicationVersionId (\ s a -> s{_daoCurrentApplicationVersionId = a}) . _Nat

-- | The ID of the configuration to delete. Each output configuration that is added to the application, either when the application is created or later using the 'AddApplicationOutput' operation, has a unique ID. You need to provide the ID to uniquely identify the output configuration that you want to delete from the application configuration. You can use the 'DescribeApplication' operation to get the specific @OutputId@ .
daoOutputId :: Lens' DeleteApplicationOutput Text
daoOutputId = lens _daoOutputId (\ s a -> s{_daoOutputId = a})

instance AWSRequest DeleteApplicationOutput where
        type Rs DeleteApplicationOutput =
             DeleteApplicationOutputResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteApplicationOutputResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteApplicationOutput where

instance NFData DeleteApplicationOutput where

instance ToHeaders DeleteApplicationOutput where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.DeleteApplicationOutput"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteApplicationOutput where
        toJSON DeleteApplicationOutput'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _daoApplicationName),
                  Just
                    ("CurrentApplicationVersionId" .=
                       _daoCurrentApplicationVersionId),
                  Just ("OutputId" .= _daoOutputId)])

instance ToPath DeleteApplicationOutput where
        toPath = const "/"

instance ToQuery DeleteApplicationOutput where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'deleteApplicationOutputResponse' smart constructor.
newtype DeleteApplicationOutputResponse = DeleteApplicationOutputResponse'
  { _daorsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteApplicationOutputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daorsResponseStatus' - -- | The response status code.
deleteApplicationOutputResponse
    :: Int -- ^ 'daorsResponseStatus'
    -> DeleteApplicationOutputResponse
deleteApplicationOutputResponse pResponseStatus_ =
  DeleteApplicationOutputResponse' {_daorsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
daorsResponseStatus :: Lens' DeleteApplicationOutputResponse Int
daorsResponseStatus = lens _daorsResponseStatus (\ s a -> s{_daorsResponseStatus = a})

instance NFData DeleteApplicationOutputResponse where
