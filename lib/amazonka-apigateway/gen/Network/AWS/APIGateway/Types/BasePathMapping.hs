{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.BasePathMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.BasePathMapping where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the base path that callers of the API must provide as part of the URL after the domain name.
--
--
-- A custom domain name plus a @BasePathMapping@ specification identifies a deployed 'RestApi' in a given stage of the owner 'Account' .<https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Custom Domain Names>
--
-- /See:/ 'basePathMapping' smart constructor.
data BasePathMapping = BasePathMapping'
  { _bpmStage :: !(Maybe Text),
    _bpmBasePath :: !(Maybe Text),
    _bpmRestAPIId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BasePathMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpmStage' - The name of the associated stage.
--
-- * 'bpmBasePath' - The base path name that callers of the API must provide as part of the URL after the domain name.
--
-- * 'bpmRestAPIId' - The string identifier of the associated 'RestApi' .
basePathMapping ::
  BasePathMapping
basePathMapping =
  BasePathMapping'
    { _bpmStage = Nothing,
      _bpmBasePath = Nothing,
      _bpmRestAPIId = Nothing
    }

-- | The name of the associated stage.
bpmStage :: Lens' BasePathMapping (Maybe Text)
bpmStage = lens _bpmStage (\s a -> s {_bpmStage = a})

-- | The base path name that callers of the API must provide as part of the URL after the domain name.
bpmBasePath :: Lens' BasePathMapping (Maybe Text)
bpmBasePath = lens _bpmBasePath (\s a -> s {_bpmBasePath = a})

-- | The string identifier of the associated 'RestApi' .
bpmRestAPIId :: Lens' BasePathMapping (Maybe Text)
bpmRestAPIId = lens _bpmRestAPIId (\s a -> s {_bpmRestAPIId = a})

instance FromJSON BasePathMapping where
  parseJSON =
    withObject
      "BasePathMapping"
      ( \x ->
          BasePathMapping'
            <$> (x .:? "stage") <*> (x .:? "basePath") <*> (x .:? "restApiId")
      )

instance Hashable BasePathMapping

instance NFData BasePathMapping
