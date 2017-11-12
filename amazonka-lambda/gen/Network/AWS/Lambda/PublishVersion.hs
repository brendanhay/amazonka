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
-- Module      : Network.AWS.Lambda.PublishVersion
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Publishes a version of your function from the current snapshot of $LATEST. That is, AWS Lambda takes a snapshot of the function code and configuration information from $LATEST and publishes a new version. The code and configuration cannot be modified after publication. For information about the versioning feature, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
--
module Network.AWS.Lambda.PublishVersion
    (
    -- * Creating a Request
      publishVersion
    , PublishVersion
    -- * Request Lenses
    , pvCodeSha256
    , pvDescription
    , pvFunctionName

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
-- /See:/ 'publishVersion' smart constructor.
data PublishVersion = PublishVersion'
  { _pvCodeSha256   :: !(Maybe Text)
  , _pvDescription  :: !(Maybe Text)
  , _pvFunctionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublishVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvCodeSha256' - The SHA256 hash of the deployment package you want to publish. This provides validation on the code you are publishing. If you provide this parameter value must match the SHA256 of the $LATEST version for the publication to succeed.
--
-- * 'pvDescription' - The description for the version you are publishing. If not provided, AWS Lambda copies the description from the $LATEST version.
--
-- * 'pvFunctionName' - The Lambda function name. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
publishVersion
    :: Text -- ^ 'pvFunctionName'
    -> PublishVersion
publishVersion pFunctionName_ =
  PublishVersion'
  { _pvCodeSha256 = Nothing
  , _pvDescription = Nothing
  , _pvFunctionName = pFunctionName_
  }


-- | The SHA256 hash of the deployment package you want to publish. This provides validation on the code you are publishing. If you provide this parameter value must match the SHA256 of the $LATEST version for the publication to succeed.
pvCodeSha256 :: Lens' PublishVersion (Maybe Text)
pvCodeSha256 = lens _pvCodeSha256 (\ s a -> s{_pvCodeSha256 = a});

-- | The description for the version you are publishing. If not provided, AWS Lambda copies the description from the $LATEST version.
pvDescription :: Lens' PublishVersion (Maybe Text)
pvDescription = lens _pvDescription (\ s a -> s{_pvDescription = a});

-- | The Lambda function name. You can specify a function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ). AWS Lambda also allows you to specify a partial ARN (for example, @account-id:Thumbnail@ ). Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
pvFunctionName :: Lens' PublishVersion Text
pvFunctionName = lens _pvFunctionName (\ s a -> s{_pvFunctionName = a});

instance AWSRequest PublishVersion where
        type Rs PublishVersion = FunctionConfiguration
        request = postJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PublishVersion where

instance NFData PublishVersion where

instance ToHeaders PublishVersion where
        toHeaders = const mempty

instance ToJSON PublishVersion where
        toJSON PublishVersion'{..}
          = object
              (catMaybes
                 [("CodeSha256" .=) <$> _pvCodeSha256,
                  ("Description" .=) <$> _pvDescription])

instance ToPath PublishVersion where
        toPath PublishVersion'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _pvFunctionName,
               "/versions"]

instance ToQuery PublishVersion where
        toQuery = const mempty
