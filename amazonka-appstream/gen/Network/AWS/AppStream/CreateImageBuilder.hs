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
-- Module      : Network.AWS.AppStream.CreateImageBuilder
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an image builder.
--
--
-- The initial state of the builder is @PENDING@ . When it is ready, the state is @RUNNING@ .
--
module Network.AWS.AppStream.CreateImageBuilder
    (
    -- * Creating a Request
      createImageBuilder
    , CreateImageBuilder
    -- * Request Lenses
    , cibDomainJoinInfo
    , cibVPCConfig
    , cibDisplayName
    , cibEnableDefaultInternetAccess
    , cibDescription
    , cibAppstreamAgentVersion
    , cibName
    , cibImageName
    , cibInstanceType

    -- * Destructuring the Response
    , createImageBuilderResponse
    , CreateImageBuilderResponse
    -- * Response Lenses
    , cibrsImageBuilder
    , cibrsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createImageBuilder' smart constructor.
data CreateImageBuilder = CreateImageBuilder'
  { _cibDomainJoinInfo              :: !(Maybe DomainJoinInfo)
  , _cibVPCConfig                   :: !(Maybe VPCConfig)
  , _cibDisplayName                 :: !(Maybe Text)
  , _cibEnableDefaultInternetAccess :: !(Maybe Bool)
  , _cibDescription                 :: !(Maybe Text)
  , _cibAppstreamAgentVersion       :: !(Maybe Text)
  , _cibName                        :: !Text
  , _cibImageName                   :: !Text
  , _cibInstanceType                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateImageBuilder' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cibDomainJoinInfo' - The information needed to join a Microsoft Active Directory domain.
--
-- * 'cibVPCConfig' - The VPC configuration for the image builder. You can specify only one subnet.
--
-- * 'cibDisplayName' - The image builder name for display.
--
-- * 'cibEnableDefaultInternetAccess' - Enables or disables default internet access for the image builder.
--
-- * 'cibDescription' - The description for display.
--
-- * 'cibAppstreamAgentVersion' - The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
--
-- * 'cibName' - A unique name for the image builder.
--
-- * 'cibImageName' - The name of the image used to create the builder.
--
-- * 'cibInstanceType' - The instance type to use when launching the image builder.
createImageBuilder
    :: Text -- ^ 'cibName'
    -> Text -- ^ 'cibImageName'
    -> Text -- ^ 'cibInstanceType'
    -> CreateImageBuilder
createImageBuilder pName_ pImageName_ pInstanceType_ =
  CreateImageBuilder'
    { _cibDomainJoinInfo = Nothing
    , _cibVPCConfig = Nothing
    , _cibDisplayName = Nothing
    , _cibEnableDefaultInternetAccess = Nothing
    , _cibDescription = Nothing
    , _cibAppstreamAgentVersion = Nothing
    , _cibName = pName_
    , _cibImageName = pImageName_
    , _cibInstanceType = pInstanceType_
    }


-- | The information needed to join a Microsoft Active Directory domain.
cibDomainJoinInfo :: Lens' CreateImageBuilder (Maybe DomainJoinInfo)
cibDomainJoinInfo = lens _cibDomainJoinInfo (\ s a -> s{_cibDomainJoinInfo = a})

-- | The VPC configuration for the image builder. You can specify only one subnet.
cibVPCConfig :: Lens' CreateImageBuilder (Maybe VPCConfig)
cibVPCConfig = lens _cibVPCConfig (\ s a -> s{_cibVPCConfig = a})

-- | The image builder name for display.
cibDisplayName :: Lens' CreateImageBuilder (Maybe Text)
cibDisplayName = lens _cibDisplayName (\ s a -> s{_cibDisplayName = a})

-- | Enables or disables default internet access for the image builder.
cibEnableDefaultInternetAccess :: Lens' CreateImageBuilder (Maybe Bool)
cibEnableDefaultInternetAccess = lens _cibEnableDefaultInternetAccess (\ s a -> s{_cibEnableDefaultInternetAccess = a})

-- | The description for display.
cibDescription :: Lens' CreateImageBuilder (Maybe Text)
cibDescription = lens _cibDescription (\ s a -> s{_cibDescription = a})

-- | The version of the AppStream 2.0 agent to use for this image builder. To use the latest version of the AppStream 2.0 agent, specify [LATEST].
cibAppstreamAgentVersion :: Lens' CreateImageBuilder (Maybe Text)
cibAppstreamAgentVersion = lens _cibAppstreamAgentVersion (\ s a -> s{_cibAppstreamAgentVersion = a})

-- | A unique name for the image builder.
cibName :: Lens' CreateImageBuilder Text
cibName = lens _cibName (\ s a -> s{_cibName = a})

-- | The name of the image used to create the builder.
cibImageName :: Lens' CreateImageBuilder Text
cibImageName = lens _cibImageName (\ s a -> s{_cibImageName = a})

-- | The instance type to use when launching the image builder.
cibInstanceType :: Lens' CreateImageBuilder Text
cibInstanceType = lens _cibInstanceType (\ s a -> s{_cibInstanceType = a})

instance AWSRequest CreateImageBuilder where
        type Rs CreateImageBuilder =
             CreateImageBuilderResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 CreateImageBuilderResponse' <$>
                   (x .?> "ImageBuilder") <*> (pure (fromEnum s)))

instance Hashable CreateImageBuilder where

instance NFData CreateImageBuilder where

instance ToHeaders CreateImageBuilder where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.CreateImageBuilder" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateImageBuilder where
        toJSON CreateImageBuilder'{..}
          = object
              (catMaybes
                 [("DomainJoinInfo" .=) <$> _cibDomainJoinInfo,
                  ("VpcConfig" .=) <$> _cibVPCConfig,
                  ("DisplayName" .=) <$> _cibDisplayName,
                  ("EnableDefaultInternetAccess" .=) <$>
                    _cibEnableDefaultInternetAccess,
                  ("Description" .=) <$> _cibDescription,
                  ("AppstreamAgentVersion" .=) <$>
                    _cibAppstreamAgentVersion,
                  Just ("Name" .= _cibName),
                  Just ("ImageName" .= _cibImageName),
                  Just ("InstanceType" .= _cibInstanceType)])

instance ToPath CreateImageBuilder where
        toPath = const "/"

instance ToQuery CreateImageBuilder where
        toQuery = const mempty

-- | /See:/ 'createImageBuilderResponse' smart constructor.
data CreateImageBuilderResponse = CreateImageBuilderResponse'
  { _cibrsImageBuilder   :: !(Maybe ImageBuilder)
  , _cibrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateImageBuilderResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cibrsImageBuilder' - Information about the image builder.
--
-- * 'cibrsResponseStatus' - -- | The response status code.
createImageBuilderResponse
    :: Int -- ^ 'cibrsResponseStatus'
    -> CreateImageBuilderResponse
createImageBuilderResponse pResponseStatus_ =
  CreateImageBuilderResponse'
    {_cibrsImageBuilder = Nothing, _cibrsResponseStatus = pResponseStatus_}


-- | Information about the image builder.
cibrsImageBuilder :: Lens' CreateImageBuilderResponse (Maybe ImageBuilder)
cibrsImageBuilder = lens _cibrsImageBuilder (\ s a -> s{_cibrsImageBuilder = a})

-- | -- | The response status code.
cibrsResponseStatus :: Lens' CreateImageBuilderResponse Int
cibrsResponseStatus = lens _cibrsResponseStatus (\ s a -> s{_cibrsResponseStatus = a})

instance NFData CreateImageBuilderResponse where
