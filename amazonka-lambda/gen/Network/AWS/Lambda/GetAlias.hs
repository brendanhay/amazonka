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
-- Returns details about a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
--
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
-- * 'gaFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'gaName' - The name of the alias.
getAlias
    :: Text -- ^ 'gaFunctionName'
    -> Text -- ^ 'gaName'
    -> GetAlias
getAlias pFunctionName_ pName_ =
  GetAlias' {_gaFunctionName = pFunctionName_, _gaName = pName_}


-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
gaFunctionName :: Lens' GetAlias Text
gaFunctionName = lens _gaFunctionName (\ s a -> s{_gaFunctionName = a})

-- | The name of the alias.
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
