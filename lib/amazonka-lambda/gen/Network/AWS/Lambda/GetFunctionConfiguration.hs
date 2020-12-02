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
-- Returns the configuration information of the Lambda function. This the same information you provided as parameters when uploading the function by using 'CreateFunction' .
--
--
-- If you are using the versioning feature, you can retrieve this information for a specific function version by using the optional @Qualifier@ parameter and specifying the function version or alias that points to it. If you don't provide it, the API returns information about the $LATEST version of the function. For more information about versioning, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- This operation requires permission for the @lambda:GetFunctionConfiguration@ operation.
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

-- |
--
--
--
-- /See:/ 'getFunctionConfiguration' smart constructor.
data GetFunctionConfiguration = GetFunctionConfiguration'
  { _gfcQualifier    :: !(Maybe Text)
  , _gfcFunctionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetFunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gfcQualifier' - Using this optional parameter you can specify a function version or an alias name. If you specify function version, the API uses qualified function ARN and returns information about the specific function version. If you specify an alias name, the API uses the alias ARN and returns information about the function version to which the alias points. If you don't specify this parameter, the API uses unqualified function ARN, and returns information about the @> LATEST@ function version.
--
-- * 'gfcFunctionName' - The name of the Lambda function for which you want to retrieve the configuration information. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
getFunctionConfiguration
    :: Text -- ^ 'gfcFunctionName'
    -> GetFunctionConfiguration
getFunctionConfiguration pFunctionName_ =
  GetFunctionConfiguration'
    {_gfcQualifier = Nothing, _gfcFunctionName = pFunctionName_}


-- | Using this optional parameter you can specify a function version or an alias name. If you specify function version, the API uses qualified function ARN and returns information about the specific function version. If you specify an alias name, the API uses the alias ARN and returns information about the function version to which the alias points. If you don't specify this parameter, the API uses unqualified function ARN, and returns information about the @> LATEST@ function version.
gfcQualifier :: Lens' GetFunctionConfiguration (Maybe Text)
gfcQualifier = lens _gfcQualifier (\ s a -> s{_gfcQualifier = a})

-- | The name of the Lambda function for which you want to retrieve the configuration information. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
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
