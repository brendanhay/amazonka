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
-- Updates the configuration of a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
--
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
-- * 'uaRoutingConfig' - The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
--
-- * 'uaFunctionVersion' - The function version that the alias invokes.
--
-- * 'uaDescription' - A description of the alias.
--
-- * 'uaRevisionId' - Only update the alias if the revision ID matches the ID that's specified. Use this option to avoid modifying an alias that has changed since you last read it.
--
-- * 'uaFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'uaName' - The name of the alias.
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


-- | The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
uaRoutingConfig :: Lens' UpdateAlias (Maybe AliasRoutingConfiguration)
uaRoutingConfig = lens _uaRoutingConfig (\ s a -> s{_uaRoutingConfig = a})

-- | The function version that the alias invokes.
uaFunctionVersion :: Lens' UpdateAlias (Maybe Text)
uaFunctionVersion = lens _uaFunctionVersion (\ s a -> s{_uaFunctionVersion = a})

-- | A description of the alias.
uaDescription :: Lens' UpdateAlias (Maybe Text)
uaDescription = lens _uaDescription (\ s a -> s{_uaDescription = a})

-- | Only update the alias if the revision ID matches the ID that's specified. Use this option to avoid modifying an alias that has changed since you last read it.
uaRevisionId :: Lens' UpdateAlias (Maybe Text)
uaRevisionId = lens _uaRevisionId (\ s a -> s{_uaRevisionId = a})

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
uaFunctionName :: Lens' UpdateAlias Text
uaFunctionName = lens _uaFunctionName (\ s a -> s{_uaFunctionName = a})

-- | The name of the alias.
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
