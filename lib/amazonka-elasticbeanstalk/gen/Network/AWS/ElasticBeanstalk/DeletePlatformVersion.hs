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
-- Module      : Network.AWS.ElasticBeanstalk.DeletePlatformVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of a custom platform.
--
--
module Network.AWS.ElasticBeanstalk.DeletePlatformVersion
    (
    -- * Creating a Request
      deletePlatformVersion
    , DeletePlatformVersion
    -- * Request Lenses
    , dpvPlatformARN

    -- * Destructuring the Response
    , deletePlatformVersionResponse
    , DeletePlatformVersionResponse
    -- * Response Lenses
    , dpvrsPlatformSummary
    , dpvrsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deletePlatformVersion' smart constructor.
newtype DeletePlatformVersion = DeletePlatformVersion'
  { _dpvPlatformARN :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePlatformVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpvPlatformARN' - The ARN of the version of the custom platform.
deletePlatformVersion
    :: DeletePlatformVersion
deletePlatformVersion = DeletePlatformVersion' {_dpvPlatformARN = Nothing}


-- | The ARN of the version of the custom platform.
dpvPlatformARN :: Lens' DeletePlatformVersion (Maybe Text)
dpvPlatformARN = lens _dpvPlatformARN (\ s a -> s{_dpvPlatformARN = a})

instance AWSRequest DeletePlatformVersion where
        type Rs DeletePlatformVersion =
             DeletePlatformVersionResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper "DeletePlatformVersionResult"
              (\ s h x ->
                 DeletePlatformVersionResponse' <$>
                   (x .@? "PlatformSummary") <*> (pure (fromEnum s)))

instance Hashable DeletePlatformVersion where

instance NFData DeletePlatformVersion where

instance ToHeaders DeletePlatformVersion where
        toHeaders = const mempty

instance ToPath DeletePlatformVersion where
        toPath = const "/"

instance ToQuery DeletePlatformVersion where
        toQuery DeletePlatformVersion'{..}
          = mconcat
              ["Action" =: ("DeletePlatformVersion" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "PlatformArn" =: _dpvPlatformARN]

-- | /See:/ 'deletePlatformVersionResponse' smart constructor.
data DeletePlatformVersionResponse = DeletePlatformVersionResponse'
  { _dpvrsPlatformSummary :: !(Maybe PlatformSummary)
  , _dpvrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeletePlatformVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpvrsPlatformSummary' - Detailed information about the version of the custom platform.
--
-- * 'dpvrsResponseStatus' - -- | The response status code.
deletePlatformVersionResponse
    :: Int -- ^ 'dpvrsResponseStatus'
    -> DeletePlatformVersionResponse
deletePlatformVersionResponse pResponseStatus_ =
  DeletePlatformVersionResponse'
    {_dpvrsPlatformSummary = Nothing, _dpvrsResponseStatus = pResponseStatus_}


-- | Detailed information about the version of the custom platform.
dpvrsPlatformSummary :: Lens' DeletePlatformVersionResponse (Maybe PlatformSummary)
dpvrsPlatformSummary = lens _dpvrsPlatformSummary (\ s a -> s{_dpvrsPlatformSummary = a})

-- | -- | The response status code.
dpvrsResponseStatus :: Lens' DeletePlatformVersionResponse Int
dpvrsResponseStatus = lens _dpvrsResponseStatus (\ s a -> s{_dpvrsResponseStatus = a})

instance NFData DeletePlatformVersionResponse where
