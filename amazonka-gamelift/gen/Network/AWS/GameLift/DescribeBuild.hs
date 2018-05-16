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
-- Module      : Network.AWS.GameLift.DescribeBuild
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties for a build. To request a build record, specify a build ID. If successful, an object containing the build properties is returned.
--
--
-- Build-related operations include:
--
--     * 'CreateBuild'
--
--     * 'ListBuilds'
--
--     * 'DescribeBuild'
--
--     * 'UpdateBuild'
--
--     * 'DeleteBuild'
--
--
--
module Network.AWS.GameLift.DescribeBuild
    (
    -- * Creating a Request
      describeBuild
    , DescribeBuild
    -- * Request Lenses
    , dBuildId

    -- * Destructuring the Response
    , describeBuildResponse
    , DescribeBuildResponse
    -- * Response Lenses
    , dbrsBuild
    , dbrsResponseStatus
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'describeBuild' smart constructor.
newtype DescribeBuild = DescribeBuild'
  { _dBuildId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBuild' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dBuildId' - Unique identifier for a build to retrieve properties for.
describeBuild
    :: Text -- ^ 'dBuildId'
    -> DescribeBuild
describeBuild pBuildId_ = DescribeBuild' {_dBuildId = pBuildId_}


-- | Unique identifier for a build to retrieve properties for.
dBuildId :: Lens' DescribeBuild Text
dBuildId = lens _dBuildId (\ s a -> s{_dBuildId = a})

instance AWSRequest DescribeBuild where
        type Rs DescribeBuild = DescribeBuildResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeBuildResponse' <$>
                   (x .?> "Build") <*> (pure (fromEnum s)))

instance Hashable DescribeBuild where

instance NFData DescribeBuild where

instance ToHeaders DescribeBuild where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeBuild" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeBuild where
        toJSON DescribeBuild'{..}
          = object (catMaybes [Just ("BuildId" .= _dBuildId)])

instance ToPath DescribeBuild where
        toPath = const "/"

instance ToQuery DescribeBuild where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeBuildResponse' smart constructor.
data DescribeBuildResponse = DescribeBuildResponse'
  { _dbrsBuild          :: !(Maybe Build)
  , _dbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeBuildResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbrsBuild' - Set of properties describing the requested build.
--
-- * 'dbrsResponseStatus' - -- | The response status code.
describeBuildResponse
    :: Int -- ^ 'dbrsResponseStatus'
    -> DescribeBuildResponse
describeBuildResponse pResponseStatus_ =
  DescribeBuildResponse'
    {_dbrsBuild = Nothing, _dbrsResponseStatus = pResponseStatus_}


-- | Set of properties describing the requested build.
dbrsBuild :: Lens' DescribeBuildResponse (Maybe Build)
dbrsBuild = lens _dbrsBuild (\ s a -> s{_dbrsBuild = a})

-- | -- | The response status code.
dbrsResponseStatus :: Lens' DescribeBuildResponse Int
dbrsResponseStatus = lens _dbrsResponseStatus (\ s a -> s{_dbrsResponseStatus = a})

instance NFData DescribeBuildResponse where
