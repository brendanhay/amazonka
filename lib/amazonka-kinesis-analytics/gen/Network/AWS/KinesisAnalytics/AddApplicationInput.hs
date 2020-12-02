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
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationInput
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a streaming source to your Amazon Kinesis application. For conceptual information, see <http://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> .
--
--
-- You can add a streaming source either when you create an application or you can use this operation to add a streaming source after you create an application. For more information, see 'CreateApplication' .
--
-- Any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the 'DescribeApplication' operation to find the current application version.
--
-- This operation requires permissions to perform the @kinesisanalytics:AddApplicationInput@ action.
--
module Network.AWS.KinesisAnalytics.AddApplicationInput
    (
    -- * Creating a Request
      addApplicationInput
    , AddApplicationInput
    -- * Request Lenses
    , aaiApplicationName
    , aaiCurrentApplicationVersionId
    , aaiInput

    -- * Destructuring the Response
    , addApplicationInputResponse
    , AddApplicationInputResponse
    -- * Response Lenses
    , aairsResponseStatus
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
-- /See:/ 'addApplicationInput' smart constructor.
data AddApplicationInput = AddApplicationInput'
  { _aaiApplicationName             :: !Text
  , _aaiCurrentApplicationVersionId :: !Nat
  , _aaiInput                       :: !Input
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddApplicationInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aaiApplicationName' - Name of your existing Amazon Kinesis Analytics application to which you want to add the streaming source.
--
-- * 'aaiCurrentApplicationVersionId' - Current version of your Amazon Kinesis Analytics application. You can use the 'DescribeApplication' operation to find the current application version.
--
-- * 'aaiInput' - The 'Input' to add.
addApplicationInput
    :: Text -- ^ 'aaiApplicationName'
    -> Natural -- ^ 'aaiCurrentApplicationVersionId'
    -> Input -- ^ 'aaiInput'
    -> AddApplicationInput
addApplicationInput pApplicationName_ pCurrentApplicationVersionId_ pInput_ =
  AddApplicationInput'
    { _aaiApplicationName = pApplicationName_
    , _aaiCurrentApplicationVersionId = _Nat # pCurrentApplicationVersionId_
    , _aaiInput = pInput_
    }


-- | Name of your existing Amazon Kinesis Analytics application to which you want to add the streaming source.
aaiApplicationName :: Lens' AddApplicationInput Text
aaiApplicationName = lens _aaiApplicationName (\ s a -> s{_aaiApplicationName = a})

-- | Current version of your Amazon Kinesis Analytics application. You can use the 'DescribeApplication' operation to find the current application version.
aaiCurrentApplicationVersionId :: Lens' AddApplicationInput Natural
aaiCurrentApplicationVersionId = lens _aaiCurrentApplicationVersionId (\ s a -> s{_aaiCurrentApplicationVersionId = a}) . _Nat

-- | The 'Input' to add.
aaiInput :: Lens' AddApplicationInput Input
aaiInput = lens _aaiInput (\ s a -> s{_aaiInput = a})

instance AWSRequest AddApplicationInput where
        type Rs AddApplicationInput =
             AddApplicationInputResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 AddApplicationInputResponse' <$> (pure (fromEnum s)))

instance Hashable AddApplicationInput where

instance NFData AddApplicationInput where

instance ToHeaders AddApplicationInput where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.AddApplicationInput" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddApplicationInput where
        toJSON AddApplicationInput'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _aaiApplicationName),
                  Just
                    ("CurrentApplicationVersionId" .=
                       _aaiCurrentApplicationVersionId),
                  Just ("Input" .= _aaiInput)])

instance ToPath AddApplicationInput where
        toPath = const "/"

instance ToQuery AddApplicationInput where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'addApplicationInputResponse' smart constructor.
newtype AddApplicationInputResponse = AddApplicationInputResponse'
  { _aairsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddApplicationInputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aairsResponseStatus' - -- | The response status code.
addApplicationInputResponse
    :: Int -- ^ 'aairsResponseStatus'
    -> AddApplicationInputResponse
addApplicationInputResponse pResponseStatus_ =
  AddApplicationInputResponse' {_aairsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
aairsResponseStatus :: Lens' AddApplicationInputResponse Int
aairsResponseStatus = lens _aairsResponseStatus (\ s a -> s{_aairsResponseStatus = a})

instance NFData AddApplicationInputResponse where
