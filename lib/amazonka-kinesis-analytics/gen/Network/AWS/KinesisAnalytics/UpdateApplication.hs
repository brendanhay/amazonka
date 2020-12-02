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
-- Module      : Network.AWS.KinesisAnalytics.UpdateApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Kinesis Analytics application. Using this API, you can update application code, input configuration, and output configuration.
--
--
-- Note that Amazon Kinesis Analytics updates the @CurrentApplicationVersionId@ each time you update your application.
--
-- This operation requires permission for the @kinesisanalytics:UpdateApplication@ action.
--
module Network.AWS.KinesisAnalytics.UpdateApplication
    (
    -- * Creating a Request
      updateApplication
    , UpdateApplication
    -- * Request Lenses
    , uaApplicationName
    , uaCurrentApplicationVersionId
    , uaApplicationUpdate

    -- * Destructuring the Response
    , updateApplicationResponse
    , UpdateApplicationResponse
    -- * Response Lenses
    , uarsResponseStatus
    ) where

import Network.AWS.KinesisAnalytics.Types
import Network.AWS.KinesisAnalytics.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { _uaApplicationName             :: !Text
  , _uaCurrentApplicationVersionId :: !Nat
  , _uaApplicationUpdate           :: !ApplicationUpdate
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaApplicationName' - Name of the Amazon Kinesis Analytics application to update.
--
-- * 'uaCurrentApplicationVersionId' - The current application version ID. You can use the 'DescribeApplication' operation to get this value.
--
-- * 'uaApplicationUpdate' - Describes application updates.
updateApplication
    :: Text -- ^ 'uaApplicationName'
    -> Natural -- ^ 'uaCurrentApplicationVersionId'
    -> ApplicationUpdate -- ^ 'uaApplicationUpdate'
    -> UpdateApplication
updateApplication pApplicationName_ pCurrentApplicationVersionId_ pApplicationUpdate_ =
  UpdateApplication'
    { _uaApplicationName = pApplicationName_
    , _uaCurrentApplicationVersionId = _Nat # pCurrentApplicationVersionId_
    , _uaApplicationUpdate = pApplicationUpdate_
    }


-- | Name of the Amazon Kinesis Analytics application to update.
uaApplicationName :: Lens' UpdateApplication Text
uaApplicationName = lens _uaApplicationName (\ s a -> s{_uaApplicationName = a})

-- | The current application version ID. You can use the 'DescribeApplication' operation to get this value.
uaCurrentApplicationVersionId :: Lens' UpdateApplication Natural
uaCurrentApplicationVersionId = lens _uaCurrentApplicationVersionId (\ s a -> s{_uaCurrentApplicationVersionId = a}) . _Nat

-- | Describes application updates.
uaApplicationUpdate :: Lens' UpdateApplication ApplicationUpdate
uaApplicationUpdate = lens _uaApplicationUpdate (\ s a -> s{_uaApplicationUpdate = a})

instance AWSRequest UpdateApplication where
        type Rs UpdateApplication = UpdateApplicationResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateApplicationResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateApplication where

instance NFData UpdateApplication where

instance ToHeaders UpdateApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.UpdateApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateApplication where
        toJSON UpdateApplication'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _uaApplicationName),
                  Just
                    ("CurrentApplicationVersionId" .=
                       _uaCurrentApplicationVersionId),
                  Just ("ApplicationUpdate" .= _uaApplicationUpdate)])

instance ToPath UpdateApplication where
        toPath = const "/"

instance ToQuery UpdateApplication where
        toQuery = const mempty

-- | /See:/ 'updateApplicationResponse' smart constructor.
newtype UpdateApplicationResponse = UpdateApplicationResponse'
  { _uarsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uarsResponseStatus' - -- | The response status code.
updateApplicationResponse
    :: Int -- ^ 'uarsResponseStatus'
    -> UpdateApplicationResponse
updateApplicationResponse pResponseStatus_ =
  UpdateApplicationResponse' {_uarsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uarsResponseStatus :: Lens' UpdateApplicationResponse Int
uarsResponseStatus = lens _uarsResponseStatus (\ s a -> s{_uarsResponseStatus = a})

instance NFData UpdateApplicationResponse where
