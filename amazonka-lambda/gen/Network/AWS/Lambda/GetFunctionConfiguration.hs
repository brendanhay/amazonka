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
-- Module      : Network.AWS.Lambda.GetFunctionConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the version-specific settings of a Lambda function or version. The output includes only options that can vary between versions of a function. To modify these settings, use 'UpdateFunctionConfiguration' .
--
--
-- To get all of a function's details, including function-level settings, use 'GetFunction' .
--
module Network.AWS.Lambda.GetFunctionConfiguration
    (
    -- * Creating a Request
      getFunctionConfiguration
    , GetFunctionConfiguration
    -- * Request Lenses
    , gfcQualifier
    , gfcFunctionName

    -- * Destructuring the Response
    , functionConfiguration
    , FunctionConfiguration
    -- * Response Lenses
    , fcMemorySize
    , fcRuntime
    , fcFunctionARN
    , fcKMSKeyARN
    , fcEnvironment
    , fcDeadLetterConfig
    , fcRole
    , fcVPCConfig
    , fcVersion
    , fcFunctionName
    , fcLayers
    , fcCodeSize
    , fcHandler
    , fcTimeout
    , fcLastModified
    , fcCodeSha256
    , fcTracingConfig
    , fcDescription
    , fcRevisionId
    , fcMasterARN
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getFunctionConfiguration' smart constructor.
data GetFunctionConfiguration = GetFunctionConfiguration'
  { _gfcQualifier    :: !(Maybe Text)
  , _gfcFunctionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfcQualifier' - Specify a version or alias to get details about a published version of the function.
--
-- * 'gfcFunctionName' - The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
getFunctionConfiguration
    :: Text -- ^ 'gfcFunctionName'
    -> GetFunctionConfiguration
getFunctionConfiguration pFunctionName_ =
  GetFunctionConfiguration'
    {_gfcQualifier = Nothing, _gfcFunctionName = pFunctionName_}


-- | Specify a version or alias to get details about a published version of the function.
gfcQualifier :: Lens' GetFunctionConfiguration (Maybe Text)
gfcQualifier = lens _gfcQualifier (\ s a -> s{_gfcQualifier = a})

-- | The name of the Lambda function, version, or alias. __Name formats__      * __Function name__ - @my-function@ (name-only), @my-function:v1@ (with alias).     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . You can append a version number or alias to any of the formats. The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
gfcFunctionName :: Lens' GetFunctionConfiguration Text
gfcFunctionName = lens _gfcFunctionName (\ s a -> s{_gfcFunctionName = a})

instance AWSRequest GetFunctionConfiguration where
        type Rs GetFunctionConfiguration =
             FunctionConfiguration
        request = get lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetFunctionConfiguration where

instance NFData GetFunctionConfiguration where

instance ToHeaders GetFunctionConfiguration where
        toHeaders = const mempty

instance ToPath GetFunctionConfiguration where
        toPath GetFunctionConfiguration'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _gfcFunctionName,
               "/configuration"]

instance ToQuery GetFunctionConfiguration where
        toQuery GetFunctionConfiguration'{..}
          = mconcat ["Qualifier" =: _gfcQualifier]
