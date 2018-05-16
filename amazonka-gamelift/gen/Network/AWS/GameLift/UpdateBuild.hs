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
-- Module      : Network.AWS.GameLift.UpdateBuild
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates metadata in a build record, including the build name and version. To update the metadata, specify the build ID to update and provide the new values. If successful, a build object containing the updated metadata is returned.
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
module Network.AWS.GameLift.UpdateBuild
    (
    -- * Creating a Request
      updateBuild
    , UpdateBuild
    -- * Request Lenses
    , ubName
    , ubVersion
    , ubBuildId

    -- * Destructuring the Response
    , updateBuildResponse
    , UpdateBuildResponse
    -- * Response Lenses
    , ubrsBuild
    , ubrsResponseStatus
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
-- /See:/ 'updateBuild' smart constructor.
data UpdateBuild = UpdateBuild'
  { _ubName    :: !(Maybe Text)
  , _ubVersion :: !(Maybe Text)
  , _ubBuildId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBuild' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubName' - Descriptive label that is associated with a build. Build names do not need to be unique.
--
-- * 'ubVersion' - Version that is associated with this build. Version strings do not need to be unique.
--
-- * 'ubBuildId' - Unique identifier for a build to update.
updateBuild
    :: Text -- ^ 'ubBuildId'
    -> UpdateBuild
updateBuild pBuildId_ =
  UpdateBuild' {_ubName = Nothing, _ubVersion = Nothing, _ubBuildId = pBuildId_}


-- | Descriptive label that is associated with a build. Build names do not need to be unique.
ubName :: Lens' UpdateBuild (Maybe Text)
ubName = lens _ubName (\ s a -> s{_ubName = a})

-- | Version that is associated with this build. Version strings do not need to be unique.
ubVersion :: Lens' UpdateBuild (Maybe Text)
ubVersion = lens _ubVersion (\ s a -> s{_ubVersion = a})

-- | Unique identifier for a build to update.
ubBuildId :: Lens' UpdateBuild Text
ubBuildId = lens _ubBuildId (\ s a -> s{_ubBuildId = a})

instance AWSRequest UpdateBuild where
        type Rs UpdateBuild = UpdateBuildResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 UpdateBuildResponse' <$>
                   (x .?> "Build") <*> (pure (fromEnum s)))

instance Hashable UpdateBuild where

instance NFData UpdateBuild where

instance ToHeaders UpdateBuild where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.UpdateBuild" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateBuild where
        toJSON UpdateBuild'{..}
          = object
              (catMaybes
                 [("Name" .=) <$> _ubName,
                  ("Version" .=) <$> _ubVersion,
                  Just ("BuildId" .= _ubBuildId)])

instance ToPath UpdateBuild where
        toPath = const "/"

instance ToQuery UpdateBuild where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'updateBuildResponse' smart constructor.
data UpdateBuildResponse = UpdateBuildResponse'
  { _ubrsBuild          :: !(Maybe Build)
  , _ubrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateBuildResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ubrsBuild' - Object that contains the updated build record.
--
-- * 'ubrsResponseStatus' - -- | The response status code.
updateBuildResponse
    :: Int -- ^ 'ubrsResponseStatus'
    -> UpdateBuildResponse
updateBuildResponse pResponseStatus_ =
  UpdateBuildResponse'
    {_ubrsBuild = Nothing, _ubrsResponseStatus = pResponseStatus_}


-- | Object that contains the updated build record.
ubrsBuild :: Lens' UpdateBuildResponse (Maybe Build)
ubrsBuild = lens _ubrsBuild (\ s a -> s{_ubrsBuild = a})

-- | -- | The response status code.
ubrsResponseStatus :: Lens' UpdateBuildResponse Int
ubrsResponseStatus = lens _ubrsResponseStatus (\ s a -> s{_ubrsResponseStatus = a})

instance NFData UpdateBuildResponse where
