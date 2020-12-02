{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AliasConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AliasConfiguration where

import Network.AWS.Lambda.Types.AliasRoutingConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides configuration information about a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
--
--
--
-- /See:/ 'aliasConfiguration' smart constructor.
data AliasConfiguration = AliasConfiguration'
  { _acRoutingConfig ::
      !(Maybe AliasRoutingConfiguration),
    _acName :: !(Maybe Text),
    _acFunctionVersion :: !(Maybe Text),
    _acAliasARN :: !(Maybe Text),
    _acDescription :: !(Maybe Text),
    _acRevisionId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AliasConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acRoutingConfig' - The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
--
-- * 'acName' - The name of the alias.
--
-- * 'acFunctionVersion' - The function version that the alias invokes.
--
-- * 'acAliasARN' - The Amazon Resource Name (ARN) of the alias.
--
-- * 'acDescription' - A description of the alias.
--
-- * 'acRevisionId' - A unique identifier that changes when you update the alias.
aliasConfiguration ::
  AliasConfiguration
aliasConfiguration =
  AliasConfiguration'
    { _acRoutingConfig = Nothing,
      _acName = Nothing,
      _acFunctionVersion = Nothing,
      _acAliasARN = Nothing,
      _acDescription = Nothing,
      _acRevisionId = Nothing
    }

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
acRoutingConfig :: Lens' AliasConfiguration (Maybe AliasRoutingConfiguration)
acRoutingConfig = lens _acRoutingConfig (\s a -> s {_acRoutingConfig = a})

-- | The name of the alias.
acName :: Lens' AliasConfiguration (Maybe Text)
acName = lens _acName (\s a -> s {_acName = a})

-- | The function version that the alias invokes.
acFunctionVersion :: Lens' AliasConfiguration (Maybe Text)
acFunctionVersion = lens _acFunctionVersion (\s a -> s {_acFunctionVersion = a})

-- | The Amazon Resource Name (ARN) of the alias.
acAliasARN :: Lens' AliasConfiguration (Maybe Text)
acAliasARN = lens _acAliasARN (\s a -> s {_acAliasARN = a})

-- | A description of the alias.
acDescription :: Lens' AliasConfiguration (Maybe Text)
acDescription = lens _acDescription (\s a -> s {_acDescription = a})

-- | A unique identifier that changes when you update the alias.
acRevisionId :: Lens' AliasConfiguration (Maybe Text)
acRevisionId = lens _acRevisionId (\s a -> s {_acRevisionId = a})

instance FromJSON AliasConfiguration where
  parseJSON =
    withObject
      "AliasConfiguration"
      ( \x ->
          AliasConfiguration'
            <$> (x .:? "RoutingConfig")
            <*> (x .:? "Name")
            <*> (x .:? "FunctionVersion")
            <*> (x .:? "AliasArn")
            <*> (x .:? "Description")
            <*> (x .:? "RevisionId")
      )

instance Hashable AliasConfiguration

instance NFData AliasConfiguration
