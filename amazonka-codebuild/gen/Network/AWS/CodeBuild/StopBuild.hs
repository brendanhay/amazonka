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
-- Module      : Network.AWS.CodeBuild.StopBuild
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to stop running a build.
--
--
module Network.AWS.CodeBuild.StopBuild
    (
    -- * Creating a Request
      stopBuild
    , StopBuild
    -- * Request Lenses
    , sbId

    -- * Destructuring the Response
    , stopBuildResponse
    , StopBuildResponse
    -- * Response Lenses
    , sbrsBuild
    , sbrsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopBuild' smart constructor.
newtype StopBuild = StopBuild'
  { _sbId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopBuild' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbId' - The ID of the build.
stopBuild
    :: Text -- ^ 'sbId'
    -> StopBuild
stopBuild pId_ = StopBuild' {_sbId = pId_}


-- | The ID of the build.
sbId :: Lens' StopBuild Text
sbId = lens _sbId (\ s a -> s{_sbId = a})

instance AWSRequest StopBuild where
        type Rs StopBuild = StopBuildResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 StopBuildResponse' <$>
                   (x .?> "build") <*> (pure (fromEnum s)))

instance Hashable StopBuild where

instance NFData StopBuild where

instance ToHeaders StopBuild where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.StopBuild" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopBuild where
        toJSON StopBuild'{..}
          = object (catMaybes [Just ("id" .= _sbId)])

instance ToPath StopBuild where
        toPath = const "/"

instance ToQuery StopBuild where
        toQuery = const mempty

-- | /See:/ 'stopBuildResponse' smart constructor.
data StopBuildResponse = StopBuildResponse'
  { _sbrsBuild          :: !(Maybe Build)
  , _sbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopBuildResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbrsBuild' - Information about the build.
--
-- * 'sbrsResponseStatus' - -- | The response status code.
stopBuildResponse
    :: Int -- ^ 'sbrsResponseStatus'
    -> StopBuildResponse
stopBuildResponse pResponseStatus_ =
  StopBuildResponse'
    {_sbrsBuild = Nothing, _sbrsResponseStatus = pResponseStatus_}


-- | Information about the build.
sbrsBuild :: Lens' StopBuildResponse (Maybe Build)
sbrsBuild = lens _sbrsBuild (\ s a -> s{_sbrsBuild = a})

-- | -- | The response status code.
sbrsResponseStatus :: Lens' StopBuildResponse Int
sbrsResponseStatus = lens _sbrsResponseStatus (\ s a -> s{_sbrsResponseStatus = a})

instance NFData StopBuildResponse where
