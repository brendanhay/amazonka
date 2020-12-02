{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelClientConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelClientConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configures the timeout and maximum number of retries for processing a transform job invocation.
--
--
--
-- /See:/ 'modelClientConfig' smart constructor.
data ModelClientConfig = ModelClientConfig'
  { _mccInvocationsTimeoutInSeconds ::
      !(Maybe Nat),
    _mccInvocationsMaxRetries :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModelClientConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mccInvocationsTimeoutInSeconds' - The timeout value in seconds for an invocation request.
--
-- * 'mccInvocationsMaxRetries' - The maximum number of retries when invocation requests are failing.
modelClientConfig ::
  ModelClientConfig
modelClientConfig =
  ModelClientConfig'
    { _mccInvocationsTimeoutInSeconds = Nothing,
      _mccInvocationsMaxRetries = Nothing
    }

-- | The timeout value in seconds for an invocation request.
mccInvocationsTimeoutInSeconds :: Lens' ModelClientConfig (Maybe Natural)
mccInvocationsTimeoutInSeconds = lens _mccInvocationsTimeoutInSeconds (\s a -> s {_mccInvocationsTimeoutInSeconds = a}) . mapping _Nat

-- | The maximum number of retries when invocation requests are failing.
mccInvocationsMaxRetries :: Lens' ModelClientConfig (Maybe Natural)
mccInvocationsMaxRetries = lens _mccInvocationsMaxRetries (\s a -> s {_mccInvocationsMaxRetries = a}) . mapping _Nat

instance FromJSON ModelClientConfig where
  parseJSON =
    withObject
      "ModelClientConfig"
      ( \x ->
          ModelClientConfig'
            <$> (x .:? "InvocationsTimeoutInSeconds")
            <*> (x .:? "InvocationsMaxRetries")
      )

instance Hashable ModelClientConfig

instance NFData ModelClientConfig

instance ToJSON ModelClientConfig where
  toJSON ModelClientConfig' {..} =
    object
      ( catMaybes
          [ ("InvocationsTimeoutInSeconds" .=)
              <$> _mccInvocationsTimeoutInSeconds,
            ("InvocationsMaxRetries" .=) <$> _mccInvocationsMaxRetries
          ]
      )
