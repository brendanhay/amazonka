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
-- Module      : Network.AWS.KinesisAnalytics.StopApplication
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the application from processing input data. You can stop an application only if it is in the running state. You can use the 'DescribeApplication' operation to find the application state. After the application is stopped, Amazon Kinesis Analytics stops reading data from the input, the application stops processing data, and there is no output written to the destination.
--
--
-- This operation requires permissions to perform the @kinesisanalytics:StopApplication@ action.
--
module Network.AWS.KinesisAnalytics.StopApplication
    (
    -- * Creating a Request
      stopApplication
    , StopApplication
    -- * Request Lenses
    , sApplicationName

    -- * Destructuring the Response
    , stopApplicationResponse
    , StopApplicationResponse
    -- * Response Lenses
    , srsResponseStatus
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
-- /See:/ 'stopApplication' smart constructor.
newtype StopApplication = StopApplication'
  { _sApplicationName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopApplication' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sApplicationName' - Name of the running application to stop.
stopApplication
    :: Text -- ^ 'sApplicationName'
    -> StopApplication
stopApplication pApplicationName_ =
  StopApplication' {_sApplicationName = pApplicationName_}


-- | Name of the running application to stop.
sApplicationName :: Lens' StopApplication Text
sApplicationName = lens _sApplicationName (\ s a -> s{_sApplicationName = a})

instance AWSRequest StopApplication where
        type Rs StopApplication = StopApplicationResponse
        request = postJSON kinesisAnalytics
        response
          = receiveEmpty
              (\ s h x ->
                 StopApplicationResponse' <$> (pure (fromEnum s)))

instance Hashable StopApplication where

instance NFData StopApplication where

instance ToHeaders StopApplication where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("KinesisAnalytics_20150814.StopApplication" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopApplication where
        toJSON StopApplication'{..}
          = object
              (catMaybes
                 [Just ("ApplicationName" .= _sApplicationName)])

instance ToPath StopApplication where
        toPath = const "/"

instance ToQuery StopApplication where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'stopApplicationResponse' smart constructor.
newtype StopApplicationResponse = StopApplicationResponse'
  { _srsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopApplicationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
stopApplicationResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopApplicationResponse
stopApplicationResponse pResponseStatus_ =
  StopApplicationResponse' {_srsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
srsResponseStatus :: Lens' StopApplicationResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StopApplicationResponse where
