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
-- Module      : Network.AWS.Greengrass.CreateCoreDefinitionVersion
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a core definition that has already been defined. AWS Greengrass Groups must each contain exactly 1 AWS Greengrass Core.
module Network.AWS.Greengrass.CreateCoreDefinitionVersion
    (
    -- * Creating a Request
      createCoreDefinitionVersion
    , CreateCoreDefinitionVersion
    -- * Request Lenses
    , ccdvAmznClientToken
    , ccdvCores
    , ccdvCoreDefinitionId

    -- * Destructuring the Response
    , createCoreDefinitionVersionResponse
    , CreateCoreDefinitionVersionResponse
    -- * Response Lenses
    , ccdvrsARN
    , ccdvrsCreationTimestamp
    , ccdvrsVersion
    , ccdvrsId
    , ccdvrsResponseStatus
    ) where

import           Network.AWS.Greengrass.Types
import           Network.AWS.Greengrass.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createCoreDefinitionVersion' smart constructor.
data CreateCoreDefinitionVersion = CreateCoreDefinitionVersion'
    { _ccdvAmznClientToken  :: !(Maybe Text)
    , _ccdvCores            :: !(Maybe [Core])
    , _ccdvCoreDefinitionId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCoreDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccdvAmznClientToken' - The client token used to request idempotent operations.
--
-- * 'ccdvCores' - Cores in the definition version.
--
-- * 'ccdvCoreDefinitionId' - core definition Id
createCoreDefinitionVersion
    :: Text -- ^ 'ccdvCoreDefinitionId'
    -> CreateCoreDefinitionVersion
createCoreDefinitionVersion pCoreDefinitionId_ =
    CreateCoreDefinitionVersion'
    { _ccdvAmznClientToken = Nothing
    , _ccdvCores = Nothing
    , _ccdvCoreDefinitionId = pCoreDefinitionId_
    }

-- | The client token used to request idempotent operations.
ccdvAmznClientToken :: Lens' CreateCoreDefinitionVersion (Maybe Text)
ccdvAmznClientToken = lens _ccdvAmznClientToken (\ s a -> s{_ccdvAmznClientToken = a});

-- | Cores in the definition version.
ccdvCores :: Lens' CreateCoreDefinitionVersion [Core]
ccdvCores = lens _ccdvCores (\ s a -> s{_ccdvCores = a}) . _Default . _Coerce;

-- | core definition Id
ccdvCoreDefinitionId :: Lens' CreateCoreDefinitionVersion Text
ccdvCoreDefinitionId = lens _ccdvCoreDefinitionId (\ s a -> s{_ccdvCoreDefinitionId = a});

instance AWSRequest CreateCoreDefinitionVersion where
        type Rs CreateCoreDefinitionVersion =
             CreateCoreDefinitionVersionResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateCoreDefinitionVersionResponse' <$>
                   (x .?> "Arn") <*> (x .?> "CreationTimestamp") <*>
                     (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable CreateCoreDefinitionVersion

instance NFData CreateCoreDefinitionVersion

instance ToHeaders CreateCoreDefinitionVersion where
        toHeaders CreateCoreDefinitionVersion'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _ccdvAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateCoreDefinitionVersion where
        toJSON CreateCoreDefinitionVersion'{..}
          = object (catMaybes [("Cores" .=) <$> _ccdvCores])

instance ToPath CreateCoreDefinitionVersion where
        toPath CreateCoreDefinitionVersion'{..}
          = mconcat
              ["/greengrass/definition/cores/",
               toBS _ccdvCoreDefinitionId, "/versions"]

instance ToQuery CreateCoreDefinitionVersion where
        toQuery = const mempty

-- | /See:/ 'createCoreDefinitionVersionResponse' smart constructor.
data CreateCoreDefinitionVersionResponse = CreateCoreDefinitionVersionResponse'
    { _ccdvrsARN               :: !(Maybe Text)
    , _ccdvrsCreationTimestamp :: !(Maybe Text)
    , _ccdvrsVersion           :: !(Maybe Text)
    , _ccdvrsId                :: !(Maybe Text)
    , _ccdvrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCoreDefinitionVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccdvrsARN' - Arn of the version.
--
-- * 'ccdvrsCreationTimestamp' - Timestamp of when the version was created.
--
-- * 'ccdvrsVersion' - Unique Id of a version.
--
-- * 'ccdvrsId' - Id of the resource container.
--
-- * 'ccdvrsResponseStatus' - -- | The response status code.
createCoreDefinitionVersionResponse
    :: Int -- ^ 'ccdvrsResponseStatus'
    -> CreateCoreDefinitionVersionResponse
createCoreDefinitionVersionResponse pResponseStatus_ =
    CreateCoreDefinitionVersionResponse'
    { _ccdvrsARN = Nothing
    , _ccdvrsCreationTimestamp = Nothing
    , _ccdvrsVersion = Nothing
    , _ccdvrsId = Nothing
    , _ccdvrsResponseStatus = pResponseStatus_
    }

-- | Arn of the version.
ccdvrsARN :: Lens' CreateCoreDefinitionVersionResponse (Maybe Text)
ccdvrsARN = lens _ccdvrsARN (\ s a -> s{_ccdvrsARN = a});

-- | Timestamp of when the version was created.
ccdvrsCreationTimestamp :: Lens' CreateCoreDefinitionVersionResponse (Maybe Text)
ccdvrsCreationTimestamp = lens _ccdvrsCreationTimestamp (\ s a -> s{_ccdvrsCreationTimestamp = a});

-- | Unique Id of a version.
ccdvrsVersion :: Lens' CreateCoreDefinitionVersionResponse (Maybe Text)
ccdvrsVersion = lens _ccdvrsVersion (\ s a -> s{_ccdvrsVersion = a});

-- | Id of the resource container.
ccdvrsId :: Lens' CreateCoreDefinitionVersionResponse (Maybe Text)
ccdvrsId = lens _ccdvrsId (\ s a -> s{_ccdvrsId = a});

-- | -- | The response status code.
ccdvrsResponseStatus :: Lens' CreateCoreDefinitionVersionResponse Int
ccdvrsResponseStatus = lens _ccdvrsResponseStatus (\ s a -> s{_ccdvrsResponseStatus = a});

instance NFData CreateCoreDefinitionVersionResponse
