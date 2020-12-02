{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.SourceAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.SourceAccessConfiguration where

import Network.AWS.Lambda.Types.SourceAccessType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@
--
--
--
-- /See:/ 'sourceAccessConfiguration' smart constructor.
data SourceAccessConfiguration = SourceAccessConfiguration'
  { _sacURI ::
      !(Maybe Text),
    _sacType :: !(Maybe SourceAccessType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SourceAccessConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sacURI' - To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@  The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- * 'sacType' - To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@  The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
sourceAccessConfiguration ::
  SourceAccessConfiguration
sourceAccessConfiguration =
  SourceAccessConfiguration' {_sacURI = Nothing, _sacType = Nothing}

-- | To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@  The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
sacURI :: Lens' SourceAccessConfiguration (Maybe Text)
sacURI = lens _sacURI (\s a -> s {_sacURI = a})

-- | To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@  The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
sacType :: Lens' SourceAccessConfiguration (Maybe SourceAccessType)
sacType = lens _sacType (\s a -> s {_sacType = a})

instance FromJSON SourceAccessConfiguration where
  parseJSON =
    withObject
      "SourceAccessConfiguration"
      ( \x ->
          SourceAccessConfiguration' <$> (x .:? "URI") <*> (x .:? "Type")
      )

instance Hashable SourceAccessConfiguration

instance NFData SourceAccessConfiguration

instance ToJSON SourceAccessConfiguration where
  toJSON SourceAccessConfiguration' {..} =
    object
      (catMaybes [("URI" .=) <$> _sacURI, ("Type" .=) <$> _sacType])
