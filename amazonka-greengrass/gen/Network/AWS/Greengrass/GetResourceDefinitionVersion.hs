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
-- Module      : Network.AWS.Greengrass.GetResourceDefinitionVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition version, including which resources are included in the version.
module Network.AWS.Greengrass.GetResourceDefinitionVersion
    (
    -- * Creating a Request
      getResourceDefinitionVersion
    , GetResourceDefinitionVersion
    -- * Request Lenses
    , grdvResourceDefinitionVersionId
    , grdvResourceDefinitionId

    -- * Destructuring the Response
    , getResourceDefinitionVersionResponse
    , GetResourceDefinitionVersionResponse
    -- * Response Lenses
    , grdvrsDefinition
    , grdvrsARN
    , grdvrsCreationTimestamp
    , grdvrsVersion
    , grdvrsId
    , grdvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getResourceDefinitionVersion' smart constructor.
data GetResourceDefinitionVersion = GetResourceDefinitionVersion'
  { _grdvResourceDefinitionVersionId :: !Text
  , _grdvResourceDefinitionId        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResourceDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdvResourceDefinitionVersionId' - The ID of the resource definition version.
--
-- * 'grdvResourceDefinitionId' - The ID of the resource definition.
getResourceDefinitionVersion
    :: Text -- ^ 'grdvResourceDefinitionVersionId'
    -> Text -- ^ 'grdvResourceDefinitionId'
    -> GetResourceDefinitionVersion
getResourceDefinitionVersion pResourceDefinitionVersionId_ pResourceDefinitionId_ =
  GetResourceDefinitionVersion'
    { _grdvResourceDefinitionVersionId = pResourceDefinitionVersionId_
    , _grdvResourceDefinitionId = pResourceDefinitionId_
    }


-- | The ID of the resource definition version.
grdvResourceDefinitionVersionId :: Lens' GetResourceDefinitionVersion Text
grdvResourceDefinitionVersionId = lens _grdvResourceDefinitionVersionId (\ s a -> s{_grdvResourceDefinitionVersionId = a})

-- | The ID of the resource definition.
grdvResourceDefinitionId :: Lens' GetResourceDefinitionVersion Text
grdvResourceDefinitionId = lens _grdvResourceDefinitionId (\ s a -> s{_grdvResourceDefinitionId = a})

instance AWSRequest GetResourceDefinitionVersion
         where
        type Rs GetResourceDefinitionVersion =
             GetResourceDefinitionVersionResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetResourceDefinitionVersionResponse' <$>
                   (x .?> "Definition") <*> (x .?> "Arn") <*>
                     (x .?> "CreationTimestamp")
                     <*> (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable GetResourceDefinitionVersion where

instance NFData GetResourceDefinitionVersion where

instance ToHeaders GetResourceDefinitionVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetResourceDefinitionVersion where
        toPath GetResourceDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/resources/",
               toBS _grdvResourceDefinitionId, "/versions/",
               toBS _grdvResourceDefinitionVersionId]

instance ToQuery GetResourceDefinitionVersion where
        toQuery = const mempty

-- | /See:/ 'getResourceDefinitionVersionResponse' smart constructor.
data GetResourceDefinitionVersionResponse = GetResourceDefinitionVersionResponse'
  { _grdvrsDefinition        :: !(Maybe ResourceDefinitionVersion)
  , _grdvrsARN               :: !(Maybe Text)
  , _grdvrsCreationTimestamp :: !(Maybe Text)
  , _grdvrsVersion           :: !(Maybe Text)
  , _grdvrsId                :: !(Maybe Text)
  , _grdvrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetResourceDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdvrsDefinition' - Information about the definition.
--
-- * 'grdvrsARN' - Arn of the resource definition version.
--
-- * 'grdvrsCreationTimestamp' - The time, in milliseconds since the epoch, when the resource definition version was created.
--
-- * 'grdvrsVersion' - The version of the resource definition version.
--
-- * 'grdvrsId' - The ID of the resource definition version.
--
-- * 'grdvrsResponseStatus' - -- | The response status code.
getResourceDefinitionVersionResponse
    :: Int -- ^ 'grdvrsResponseStatus'
    -> GetResourceDefinitionVersionResponse
getResourceDefinitionVersionResponse pResponseStatus_ =
  GetResourceDefinitionVersionResponse'
    { _grdvrsDefinition = Nothing
    , _grdvrsARN = Nothing
    , _grdvrsCreationTimestamp = Nothing
    , _grdvrsVersion = Nothing
    , _grdvrsId = Nothing
    , _grdvrsResponseStatus = pResponseStatus_
    }


-- | Information about the definition.
grdvrsDefinition :: Lens' GetResourceDefinitionVersionResponse (Maybe ResourceDefinitionVersion)
grdvrsDefinition = lens _grdvrsDefinition (\ s a -> s{_grdvrsDefinition = a})

-- | Arn of the resource definition version.
grdvrsARN :: Lens' GetResourceDefinitionVersionResponse (Maybe Text)
grdvrsARN = lens _grdvrsARN (\ s a -> s{_grdvrsARN = a})

-- | The time, in milliseconds since the epoch, when the resource definition version was created.
grdvrsCreationTimestamp :: Lens' GetResourceDefinitionVersionResponse (Maybe Text)
grdvrsCreationTimestamp = lens _grdvrsCreationTimestamp (\ s a -> s{_grdvrsCreationTimestamp = a})

-- | The version of the resource definition version.
grdvrsVersion :: Lens' GetResourceDefinitionVersionResponse (Maybe Text)
grdvrsVersion = lens _grdvrsVersion (\ s a -> s{_grdvrsVersion = a})

-- | The ID of the resource definition version.
grdvrsId :: Lens' GetResourceDefinitionVersionResponse (Maybe Text)
grdvrsId = lens _grdvrsId (\ s a -> s{_grdvrsId = a})

-- | -- | The response status code.
grdvrsResponseStatus :: Lens' GetResourceDefinitionVersionResponse Int
grdvrsResponseStatus = lens _grdvrsResponseStatus (\ s a -> s{_grdvrsResponseStatus = a})

instance NFData GetResourceDefinitionVersionResponse
         where
