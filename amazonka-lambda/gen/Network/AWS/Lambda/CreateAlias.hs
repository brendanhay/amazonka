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
-- Module      : Network.AWS.Lambda.CreateAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> for a Lambda function version. Use aliases to provide clients with a function identifier that you can update to invoke a different version.
--
--
-- You can also map an alias to split invocation requests between two versions. Use the @RoutingConfig@ parameter to specify a second version and the percentage of invocation requests that it receives.
--
module Network.AWS.Lambda.CreateAlias
    (
    -- * Creating a Request
      createAlias
    , CreateAlias
    -- * Request Lenses
    , caRoutingConfig
    , caDescription
    , caFunctionName
    , caName
    , caFunctionVersion

    -- * Destructuring the Response
    , aliasConfiguration
    , AliasConfiguration
    -- * Response Lenses
    , acRoutingConfig
    , acName
    , acFunctionVersion
    , acAliasARN
    , acDescription
    , acRevisionId
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAlias' smart constructor.
data CreateAlias = CreateAlias'
  { _caRoutingConfig   :: !(Maybe AliasRoutingConfiguration)
  , _caDescription     :: !(Maybe Text)
  , _caFunctionName    :: !Text
  , _caName            :: !Text
  , _caFunctionVersion :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caRoutingConfig' - The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
--
-- * 'caDescription' - A description of the alias.
--
-- * 'caFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'caName' - The name of the alias.
--
-- * 'caFunctionVersion' - The function version that the alias invokes.
createAlias
    :: Text -- ^ 'caFunctionName'
    -> Text -- ^ 'caName'
    -> Text -- ^ 'caFunctionVersion'
    -> CreateAlias
createAlias pFunctionName_ pName_ pFunctionVersion_ =
  CreateAlias'
    { _caRoutingConfig = Nothing
    , _caDescription = Nothing
    , _caFunctionName = pFunctionName_
    , _caName = pName_
    , _caFunctionVersion = pFunctionVersion_
    }


-- | The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
caRoutingConfig :: Lens' CreateAlias (Maybe AliasRoutingConfiguration)
caRoutingConfig = lens _caRoutingConfig (\ s a -> s{_caRoutingConfig = a})

-- | A description of the alias.
caDescription :: Lens' CreateAlias (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
caFunctionName :: Lens' CreateAlias Text
caFunctionName = lens _caFunctionName (\ s a -> s{_caFunctionName = a})

-- | The name of the alias.
caName :: Lens' CreateAlias Text
caName = lens _caName (\ s a -> s{_caName = a})

-- | The function version that the alias invokes.
caFunctionVersion :: Lens' CreateAlias Text
caFunctionVersion = lens _caFunctionVersion (\ s a -> s{_caFunctionVersion = a})

instance AWSRequest CreateAlias where
        type Rs CreateAlias = AliasConfiguration
        request = postJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateAlias where

instance NFData CreateAlias where

instance ToHeaders CreateAlias where
        toHeaders = const mempty

instance ToJSON CreateAlias where
        toJSON CreateAlias'{..}
          = object
              (catMaybes
                 [("RoutingConfig" .=) <$> _caRoutingConfig,
                  ("Description" .=) <$> _caDescription,
                  Just ("Name" .= _caName),
                  Just ("FunctionVersion" .= _caFunctionVersion)])

instance ToPath CreateAlias where
        toPath CreateAlias'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _caFunctionName,
               "/aliases"]

instance ToQuery CreateAlias where
        toQuery = const mempty
