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
-- Module      : Network.AWS.Lambda.UpdateAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Using this API you can update the function version to which the alias points and the alias description. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html Introduction to AWS Lambda Aliases> .
--
--
-- This requires permission for the lambda:UpdateAlias action.
--
module Network.AWS.Lambda.UpdateAlias
    (
    -- * Creating a Request
      updateAlias
    , UpdateAlias
    -- * Request Lenses
    , uaRoutingConfig
    , uaFunctionVersion
    , uaDescription
    , uaRevisionId
    , uaFunctionName
    , uaName

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

-- | /See:/ 'updateAlias' smart constructor.
data UpdateAlias = UpdateAlias'
  { _uaRoutingConfig   :: !(Maybe AliasRoutingConfiguration)
  , _uaFunctionVersion :: !(Maybe Text)
  , _uaDescription     :: !(Maybe Text)
  , _uaRevisionId      :: !(Maybe Text)
  , _uaFunctionName    :: !Text
  , _uaName            :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uaRoutingConfig' - Specifies an additional version your alias can point to, allowing you to dictate what percentage of traffic will invoke each version. For more information, see 'lambda-traffic-shifting-using-aliases' .
--
-- * 'uaFunctionVersion' - Using this parameter you can change the Lambda function version to which the alias points.
--
-- * 'uaDescription' - You can change the description of the alias using this parameter.
--
-- * 'uaRevisionId' - An optional value you can use to ensure you are updating the latest update of the function version or alias. If the @RevisionID@ you pass doesn't match the latest @RevisionId@ of the function or alias, it will fail with an error message, advising you to retrieve the latest function version or alias @RevisionID@ using either or .
--
-- * 'uaFunctionName' - The function name for which the alias is created. Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'uaName' - The alias name.
updateAlias
    :: Text -- ^ 'uaFunctionName'
    -> Text -- ^ 'uaName'
    -> UpdateAlias
updateAlias pFunctionName_ pName_ =
  UpdateAlias'
    { _uaRoutingConfig = Nothing
    , _uaFunctionVersion = Nothing
    , _uaDescription = Nothing
    , _uaRevisionId = Nothing
    , _uaFunctionName = pFunctionName_
    , _uaName = pName_
    }


-- | Specifies an additional version your alias can point to, allowing you to dictate what percentage of traffic will invoke each version. For more information, see 'lambda-traffic-shifting-using-aliases' .
uaRoutingConfig :: Lens' UpdateAlias (Maybe AliasRoutingConfiguration)
uaRoutingConfig = lens _uaRoutingConfig (\ s a -> s{_uaRoutingConfig = a})

-- | Using this parameter you can change the Lambda function version to which the alias points.
uaFunctionVersion :: Lens' UpdateAlias (Maybe Text)
uaFunctionVersion = lens _uaFunctionVersion (\ s a -> s{_uaFunctionVersion = a})

-- | You can change the description of the alias using this parameter.
uaDescription :: Lens' UpdateAlias (Maybe Text)
uaDescription = lens _uaDescription (\ s a -> s{_uaDescription = a})

-- | An optional value you can use to ensure you are updating the latest update of the function version or alias. If the @RevisionID@ you pass doesn't match the latest @RevisionId@ of the function or alias, it will fail with an error message, advising you to retrieve the latest function version or alias @RevisionID@ using either or .
uaRevisionId :: Lens' UpdateAlias (Maybe Text)
uaRevisionId = lens _uaRevisionId (\ s a -> s{_uaRevisionId = a})

-- | The function name for which the alias is created. Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
uaFunctionName :: Lens' UpdateAlias Text
uaFunctionName = lens _uaFunctionName (\ s a -> s{_uaFunctionName = a})

-- | The alias name.
uaName :: Lens' UpdateAlias Text
uaName = lens _uaName (\ s a -> s{_uaName = a})

instance AWSRequest UpdateAlias where
        type Rs UpdateAlias = AliasConfiguration
        request = putJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateAlias where

instance NFData UpdateAlias where

instance ToHeaders UpdateAlias where
        toHeaders = const mempty

instance ToJSON UpdateAlias where
        toJSON UpdateAlias'{..}
          = object
              (catMaybes
                 [("RoutingConfig" .=) <$> _uaRoutingConfig,
                  ("FunctionVersion" .=) <$> _uaFunctionVersion,
                  ("Description" .=) <$> _uaDescription,
                  ("RevisionId" .=) <$> _uaRevisionId])

instance ToPath UpdateAlias where
        toPath UpdateAlias'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _uaFunctionName,
               "/aliases/", toBS _uaName]

instance ToQuery UpdateAlias where
        toQuery = const mempty
