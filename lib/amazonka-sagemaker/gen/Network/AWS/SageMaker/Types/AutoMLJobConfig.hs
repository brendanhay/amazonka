{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLJobConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLJobConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
import Network.AWS.SageMaker.Types.AutoMLSecurityConfig

-- | A collection of settings used for a job.
--
--
--
-- /See:/ 'autoMLJobConfig' smart constructor.
data AutoMLJobConfig = AutoMLJobConfig'
  { _amljcSecurityConfig ::
      !(Maybe AutoMLSecurityConfig),
    _amljcCompletionCriteria ::
      !(Maybe AutoMLJobCompletionCriteria)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AutoMLJobConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'amljcSecurityConfig' - Security configuration for traffic encryption or Amazon VPC settings.
--
-- * 'amljcCompletionCriteria' - How long a job is allowed to run, or how many candidates a job is allowed to generate.
autoMLJobConfig ::
  AutoMLJobConfig
autoMLJobConfig =
  AutoMLJobConfig'
    { _amljcSecurityConfig = Nothing,
      _amljcCompletionCriteria = Nothing
    }

-- | Security configuration for traffic encryption or Amazon VPC settings.
amljcSecurityConfig :: Lens' AutoMLJobConfig (Maybe AutoMLSecurityConfig)
amljcSecurityConfig = lens _amljcSecurityConfig (\s a -> s {_amljcSecurityConfig = a})

-- | How long a job is allowed to run, or how many candidates a job is allowed to generate.
amljcCompletionCriteria :: Lens' AutoMLJobConfig (Maybe AutoMLJobCompletionCriteria)
amljcCompletionCriteria = lens _amljcCompletionCriteria (\s a -> s {_amljcCompletionCriteria = a})

instance FromJSON AutoMLJobConfig where
  parseJSON =
    withObject
      "AutoMLJobConfig"
      ( \x ->
          AutoMLJobConfig'
            <$> (x .:? "SecurityConfig") <*> (x .:? "CompletionCriteria")
      )

instance Hashable AutoMLJobConfig

instance NFData AutoMLJobConfig

instance ToJSON AutoMLJobConfig where
  toJSON AutoMLJobConfig' {..} =
    object
      ( catMaybes
          [ ("SecurityConfig" .=) <$> _amljcSecurityConfig,
            ("CompletionCriteria" .=) <$> _amljcCompletionCriteria
          ]
      )
