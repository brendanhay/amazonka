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
-- Module      : Network.AWS.Lambda.GetAlias
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specified alias information such as the alias ARN, description, and function version it is pointing to. For more information, see <http://docs.aws.amazon.com/lambda/latest/dg/aliases-intro.html Introduction to AWS Lambda Aliases> .
--
--
-- This requires permission for the @lambda:GetAlias@ action.
--
module Network.AWS.Lambda.GetAlias
    (
    -- * Creating a Request
      getAlias
    , GetAlias
    -- * Request Lenses
    , gaFunctionName
    , gaName

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

-- | /See:/ 'getAlias' smart constructor.
data GetAlias = GetAlias'
  { _gaFunctionName :: !Text
  , _gaName         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetAlias' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gaFunctionName' - Function name for which the alias is created. An alias is a subresource that exists only in the context of an existing Lambda function so you must specify the function name. Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'gaName' - Name of the alias for which you want to retrieve information.
getAlias
    :: Text -- ^ 'gaFunctionName'
    -> Text -- ^ 'gaName'
    -> GetAlias
getAlias pFunctionName_ pName_ =
  GetAlias' {_gaFunctionName = pFunctionName_, _gaName = pName_}


-- | Function name for which the alias is created. An alias is a subresource that exists only in the context of an existing Lambda function so you must specify the function name. Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
gaFunctionName :: Lens' GetAlias Text
gaFunctionName = lens _gaFunctionName (\ s a -> s{_gaFunctionName = a})

-- | Name of the alias for which you want to retrieve information.
gaName :: Lens' GetAlias Text
gaName = lens _gaName (\ s a -> s{_gaName = a})

instance AWSRequest GetAlias where
        type Rs GetAlias = AliasConfiguration
        request = get lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable GetAlias where

instance NFData GetAlias where

instance ToHeaders GetAlias where
        toHeaders = const mempty

instance ToPath GetAlias where
        toPath GetAlias'{..}
          = mconcat
              ["/2015-03-31/functions/", toBS _gaFunctionName,
               "/aliases/", toBS _gaName]

instance ToQuery GetAlias where
        toQuery = const mempty
