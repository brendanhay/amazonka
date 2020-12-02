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
-- Creates an alias that points to the specified Lambda function version. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html Introduction to AWS Lambda Aliases> .
--
--
-- Alias names are unique for a given function. This requires permission for the lambda:CreateAlias action.
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
-- * 'caRoutingConfig' - Specifies an additional version your alias can point to, allowing you to dictate what percentage of traffic will invoke each version. For more information, see 'lambda-traffic-shifting-using-aliases' .
--
-- * 'caDescription' - Description of the alias.
--
-- * 'caFunctionName' - Name of the Lambda function for which you want to create an alias. Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'caName' - Name for the alias you are creating.
--
-- * 'caFunctionVersion' - Lambda function version for which you are creating the alias.
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


-- | Specifies an additional version your alias can point to, allowing you to dictate what percentage of traffic will invoke each version. For more information, see 'lambda-traffic-shifting-using-aliases' .
caRoutingConfig :: Lens' CreateAlias (Maybe AliasRoutingConfiguration)
caRoutingConfig = lens _caRoutingConfig (\ s a -> s{_caRoutingConfig = a})

-- | Description of the alias.
caDescription :: Lens' CreateAlias (Maybe Text)
caDescription = lens _caDescription (\ s a -> s{_caDescription = a})

-- | Name of the Lambda function for which you want to create an alias. Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
caFunctionName :: Lens' CreateAlias Text
caFunctionName = lens _caFunctionName (\ s a -> s{_caFunctionName = a})

-- | Name for the alias you are creating.
caName :: Lens' CreateAlias Text
caName = lens _caName (\ s a -> s{_caName = a})

-- | Lambda function version for which you are creating the alias.
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
