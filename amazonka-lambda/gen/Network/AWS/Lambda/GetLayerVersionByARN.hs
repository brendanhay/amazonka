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
-- Module      : Network.AWS.Lambda.GetLayerVersionByARN
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> , with a link to download the layer archive that's valid for 10 minutes.
--
--
module Network.AWS.Lambda.GetLayerVersionByARN
    (
    -- * Creating a Request
      getLayerVersionByARN
    , GetLayerVersionByARN
    -- * Request Lenses
    , glvbaARN

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

-- | /See:/ 'getLayerVersionByARN' smart constructor.
newtype GetLayerVersionByARN = GetLayerVersionByARN'
  { _glvbaARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetLayerVersionByARN' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'glvbaARN' - The ARN of the layer version.
getLayerVersionByARN
    :: Text -- ^ 'glvbaARN'
    -> GetLayerVersionByARN
getLayerVersionByARN pARN_ = GetLayerVersionByARN' {_glvbaARN = pARN_}


-- | The ARN of the layer version.
glvbaARN :: Lens' GetLayerVersionByARN Text
glvbaARN = lens _glvbaARN (\ s a -> s{_glvbaARN = a})

instance AWSRequest GetLayerVersionByARN where
        type Rs GetLayerVersionByARN =
             GetLayerVersionResponse
        request = get lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetLayerVersionByARN where

instance NFData GetLayerVersionByARN where

instance ToHeaders GetLayerVersionByARN where
        toHeaders = const mempty

instance ToPath GetLayerVersionByARN where
        toPath = const "/2018-10-31/layers"

instance ToQuery GetLayerVersionByARN where
        toQuery GetLayerVersionByARN'{..}
          = mconcat ["Arn" =: _glvbaARN, "find=LayerVersion"]
