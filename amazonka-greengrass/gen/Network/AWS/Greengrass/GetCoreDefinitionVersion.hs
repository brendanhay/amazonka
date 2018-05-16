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
-- Module      : Network.AWS.Greengrass.GetCoreDefinitionVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Network.AWS.Greengrass.GetCoreDefinitionVersion
    (
    -- * Creating a Request
      getCoreDefinitionVersion
    , GetCoreDefinitionVersion
    -- * Request Lenses
    , gcdvCoreDefinitionId
    , gcdvCoreDefinitionVersionId

    -- * Destructuring the Response
    , getCoreDefinitionVersionResponse
    , GetCoreDefinitionVersionResponse
    -- * Response Lenses
    , gcdvrsDefinition
    , gcdvrsARN
    , gcdvrsCreationTimestamp
    , gcdvrsVersion
    , gcdvrsId
    , gcdvrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCoreDefinitionVersion' smart constructor.
data GetCoreDefinitionVersion = GetCoreDefinitionVersion'
  { _gcdvCoreDefinitionId        :: !Text
  , _gcdvCoreDefinitionVersionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCoreDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdvCoreDefinitionId' - The ID of the core definition.
--
-- * 'gcdvCoreDefinitionVersionId' - The ID of the core definition version.
getCoreDefinitionVersion
    :: Text -- ^ 'gcdvCoreDefinitionId'
    -> Text -- ^ 'gcdvCoreDefinitionVersionId'
    -> GetCoreDefinitionVersion
getCoreDefinitionVersion pCoreDefinitionId_ pCoreDefinitionVersionId_ =
  GetCoreDefinitionVersion'
    { _gcdvCoreDefinitionId = pCoreDefinitionId_
    , _gcdvCoreDefinitionVersionId = pCoreDefinitionVersionId_
    }


-- | The ID of the core definition.
gcdvCoreDefinitionId :: Lens' GetCoreDefinitionVersion Text
gcdvCoreDefinitionId = lens _gcdvCoreDefinitionId (\ s a -> s{_gcdvCoreDefinitionId = a})

-- | The ID of the core definition version.
gcdvCoreDefinitionVersionId :: Lens' GetCoreDefinitionVersion Text
gcdvCoreDefinitionVersionId = lens _gcdvCoreDefinitionVersionId (\ s a -> s{_gcdvCoreDefinitionVersionId = a})

instance AWSRequest GetCoreDefinitionVersion where
        type Rs GetCoreDefinitionVersion =
             GetCoreDefinitionVersionResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetCoreDefinitionVersionResponse' <$>
                   (x .?> "Definition") <*> (x .?> "Arn") <*>
                     (x .?> "CreationTimestamp")
                     <*> (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable GetCoreDefinitionVersion where

instance NFData GetCoreDefinitionVersion where

instance ToHeaders GetCoreDefinitionVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetCoreDefinitionVersion where
        toPath GetCoreDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/cores/",
               toBS _gcdvCoreDefinitionId, "/versions/",
               toBS _gcdvCoreDefinitionVersionId]

instance ToQuery GetCoreDefinitionVersion where
        toQuery = const mempty

-- | /See:/ 'getCoreDefinitionVersionResponse' smart constructor.
data GetCoreDefinitionVersionResponse = GetCoreDefinitionVersionResponse'
  { _gcdvrsDefinition        :: !(Maybe CoreDefinitionVersion)
  , _gcdvrsARN               :: !(Maybe Text)
  , _gcdvrsCreationTimestamp :: !(Maybe Text)
  , _gcdvrsVersion           :: !(Maybe Text)
  , _gcdvrsId                :: !(Maybe Text)
  , _gcdvrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetCoreDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcdvrsDefinition' - Information about the core definition version.
--
-- * 'gcdvrsARN' - The ARN of the core definition version.
--
-- * 'gcdvrsCreationTimestamp' - The time, in milliseconds since the epoch, when the core definition version was created.
--
-- * 'gcdvrsVersion' - The version of the core definition version.
--
-- * 'gcdvrsId' - The ID of the core definition version.
--
-- * 'gcdvrsResponseStatus' - -- | The response status code.
getCoreDefinitionVersionResponse
    :: Int -- ^ 'gcdvrsResponseStatus'
    -> GetCoreDefinitionVersionResponse
getCoreDefinitionVersionResponse pResponseStatus_ =
  GetCoreDefinitionVersionResponse'
    { _gcdvrsDefinition = Nothing
    , _gcdvrsARN = Nothing
    , _gcdvrsCreationTimestamp = Nothing
    , _gcdvrsVersion = Nothing
    , _gcdvrsId = Nothing
    , _gcdvrsResponseStatus = pResponseStatus_
    }


-- | Information about the core definition version.
gcdvrsDefinition :: Lens' GetCoreDefinitionVersionResponse (Maybe CoreDefinitionVersion)
gcdvrsDefinition = lens _gcdvrsDefinition (\ s a -> s{_gcdvrsDefinition = a})

-- | The ARN of the core definition version.
gcdvrsARN :: Lens' GetCoreDefinitionVersionResponse (Maybe Text)
gcdvrsARN = lens _gcdvrsARN (\ s a -> s{_gcdvrsARN = a})

-- | The time, in milliseconds since the epoch, when the core definition version was created.
gcdvrsCreationTimestamp :: Lens' GetCoreDefinitionVersionResponse (Maybe Text)
gcdvrsCreationTimestamp = lens _gcdvrsCreationTimestamp (\ s a -> s{_gcdvrsCreationTimestamp = a})

-- | The version of the core definition version.
gcdvrsVersion :: Lens' GetCoreDefinitionVersionResponse (Maybe Text)
gcdvrsVersion = lens _gcdvrsVersion (\ s a -> s{_gcdvrsVersion = a})

-- | The ID of the core definition version.
gcdvrsId :: Lens' GetCoreDefinitionVersionResponse (Maybe Text)
gcdvrsId = lens _gcdvrsId (\ s a -> s{_gcdvrsId = a})

-- | -- | The response status code.
gcdvrsResponseStatus :: Lens' GetCoreDefinitionVersionResponse Int
gcdvrsResponseStatus = lens _gcdvrsResponseStatus (\ s a -> s{_gcdvrsResponseStatus = a})

instance NFData GetCoreDefinitionVersionResponse
         where
