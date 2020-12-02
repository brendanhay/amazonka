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
-- Module      : Network.AWS.APIGateway.GetSDKType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
module Network.AWS.APIGateway.GetSDKType
    (
    -- * Creating a Request
      getSDKType
    , GetSDKType
    -- * Request Lenses
    , gstId

    -- * Destructuring the Response
    , sdkType
    , SDKType
    -- * Response Lenses
    , stFriendlyName
    , stConfigurationProperties
    , stId
    , stDescription
    ) where

import Network.AWS.APIGateway.Types
import Network.AWS.APIGateway.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Get an 'SdkType' instance.
--
--
--
-- /See:/ 'getSDKType' smart constructor.
newtype GetSDKType = GetSDKType'
  { _gstId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSDKType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gstId' - [Required] The identifier of the queried 'SdkType' instance.
getSDKType
    :: Text -- ^ 'gstId'
    -> GetSDKType
getSDKType pId_ = GetSDKType' {_gstId = pId_}


-- | [Required] The identifier of the queried 'SdkType' instance.
gstId :: Lens' GetSDKType Text
gstId = lens _gstId (\ s a -> s{_gstId = a})

instance AWSRequest GetSDKType where
        type Rs GetSDKType = SDKType
        request = get apiGateway
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetSDKType where

instance NFData GetSDKType where

instance ToHeaders GetSDKType where
        toHeaders
          = const
              (mconcat
                 ["Accept" =# ("application/json" :: ByteString)])

instance ToPath GetSDKType where
        toPath GetSDKType'{..}
          = mconcat ["/sdktypes/", toBS _gstId]

instance ToQuery GetSDKType where
        toQuery = const mempty
