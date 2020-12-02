{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The authorizer summary.
--
--
--
-- /See:/ 'authorizerSummary' smart constructor.
data AuthorizerSummary = AuthorizerSummary'
  { _asAuthorizerName ::
      !(Maybe Text),
    _asAuthorizerARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AuthorizerSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asAuthorizerName' - The authorizer name.
--
-- * 'asAuthorizerARN' - The authorizer ARN.
authorizerSummary ::
  AuthorizerSummary
authorizerSummary =
  AuthorizerSummary'
    { _asAuthorizerName = Nothing,
      _asAuthorizerARN = Nothing
    }

-- | The authorizer name.
asAuthorizerName :: Lens' AuthorizerSummary (Maybe Text)
asAuthorizerName = lens _asAuthorizerName (\s a -> s {_asAuthorizerName = a})

-- | The authorizer ARN.
asAuthorizerARN :: Lens' AuthorizerSummary (Maybe Text)
asAuthorizerARN = lens _asAuthorizerARN (\s a -> s {_asAuthorizerARN = a})

instance FromJSON AuthorizerSummary where
  parseJSON =
    withObject
      "AuthorizerSummary"
      ( \x ->
          AuthorizerSummary'
            <$> (x .:? "authorizerName") <*> (x .:? "authorizerArn")
      )

instance Hashable AuthorizerSummary

instance NFData AuthorizerSummary
