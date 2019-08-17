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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html version> from the current code and configuration of a function. Use versions to create a snapshot of your function code and configuration that doesn't change.
--
--
-- AWS Lambda doesn't publish a version if the function's configuration and code haven't changed since the last version. Use 'UpdateFunctionCode' or 'UpdateFunctionConfiguration' to update the function before publishing a version.
--
-- Clients can invoke versions directly or with an alias. To create an alias, use 'CreateAlias' .
--
module Network.AWS.Lambda.PublishVersion
    (
    -- * Creating a Request
      publishVersion
    , PublishVersion
    -- * Request Lenses
    , pvCodeSha256
    , pvDescription
    , pvRevisionId
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

-- | /See:/ 'publishVersion' smart constructor.
data PublishVersion = PublishVersion'
  { _pvCodeSha256   :: !(Maybe Text)
  , _pvDescription  :: !(Maybe Text)
  , _pvRevisionId   :: !(Maybe Text)
  , _pvFunctionName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PublishVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pvCodeSha256' - Only publish a version if the hash value matches the value that's specified. Use this option to avoid publishing a version if the function code has changed since you last updated it. You can get the hash for the version that you uploaded from the output of 'UpdateFunctionCode' .
--
-- * 'pvDescription' - A description for the version to override the description in the function configuration.
--
-- * 'pvRevisionId' - Only update the function if the revision ID matches the ID that's specified. Use this option to avoid publishing a version if the function configuration has changed since you last updated it.
--
-- * 'pvFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
publishVersion
    :: Text -- ^ 'pvFunctionName'
    -> PublishVersion
publishVersion pFunctionName_ =
  PublishVersion'
    { _pvCodeSha256 = Nothing
    , _pvDescription = Nothing
    , _pvRevisionId = Nothing
    , _pvFunctionName = pFunctionName_
    }


-- | Only publish a version if the hash value matches the value that's specified. Use this option to avoid publishing a version if the function code has changed since you last updated it. You can get the hash for the version that you uploaded from the output of 'UpdateFunctionCode' .
pvCodeSha256 :: Lens' PublishVersion (Maybe Text)
pvCodeSha256 = lens _pvCodeSha256 (\ s a -> s{_pvCodeSha256 = a})

-- | A description for the version to override the description in the function configuration.
pvDescription :: Lens' PublishVersion (Maybe Text)
pvDescription = lens _pvDescription (\ s a -> s{_pvDescription = a})

-- | Only update the function if the revision ID matches the ID that's specified. Use this option to avoid publishing a version if the function configuration has changed since you last updated it.
pvRevisionId :: Lens' PublishVersion (Maybe Text)
pvRevisionId = lens _pvRevisionId (\ s a -> s{_pvRevisionId = a})

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
pvFunctionName :: Lens' PublishVersion Text
pvFunctionName = lens _pvFunctionName (\ s a -> s{_pvFunctionName = a})

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
                  ("Description" .=) <$> _pvDescription,
                  ("RevisionId" .=) <$> _pvRevisionId])

instance ToPath PublishVersion where
        toPath PublishVersion'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _pvFunctionName,
               "/versions"]

instance ToQuery PublishVersion where
        toQuery = const mempty
