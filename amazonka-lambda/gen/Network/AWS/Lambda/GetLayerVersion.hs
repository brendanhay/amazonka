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
-- Module      : Network.AWS.Lambda.GetLayerVersion
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> , with a link to download the layer archive that's valid for 10 minutes.
--
--
module Network.AWS.Lambda.GetLayerVersion
    (
    -- * Creating a Request
      getLayerVersion
    , GetLayerVersion
    -- * Request Lenses
    , glvLayerName
    , glvVersionNumber

    -- * Destructuring the Response
    , getLayerVersionResponse
    , GetLayerVersionResponse
    -- * Response Lenses
    , glvLayerVersionARN
    , glvContent
    , glvCreatedDate
    , glvVersion
    , glvLicenseInfo
    , glvLayerARN
    , glvDescription
    , glvCompatibleRuntimes
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getLayerVersion' smart constructor.
data GetLayerVersion = GetLayerVersion'
  { _glvLayerName     :: !Text
  , _glvVersionNumber :: !Integer
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLayerVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glvLayerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- * 'glvVersionNumber' - The version number.
getLayerVersion
    :: Text -- ^ 'glvLayerName'
    -> Integer -- ^ 'glvVersionNumber'
    -> GetLayerVersion
getLayerVersion pLayerName_ pVersionNumber_ =
  GetLayerVersion'
    {_glvLayerName = pLayerName_, _glvVersionNumber = pVersionNumber_}


-- | The name or Amazon Resource Name (ARN) of the layer.
glvLayerName :: Lens' GetLayerVersion Text
glvLayerName = lens _glvLayerName (\ s a -> s{_glvLayerName = a})

-- | The version number.
glvVersionNumber :: Lens' GetLayerVersion Integer
glvVersionNumber = lens _glvVersionNumber (\ s a -> s{_glvVersionNumber = a})

instance AWSRequest GetLayerVersion where
        type Rs GetLayerVersion = GetLayerVersionResponse
        request = get lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetLayerVersion where

instance NFData GetLayerVersion where

instance ToHeaders GetLayerVersion where
        toHeaders = const mempty

instance ToPath GetLayerVersion where
        toPath GetLayerVersion'{..}
          = mconcat
              ["/2018-10-31/layers/", toBS _glvLayerName,
               "/versions/", toBS _glvVersionNumber]

instance ToQuery GetLayerVersion where
        toQuery = const mempty
