{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.AppValidationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.AppValidationOutput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.SSMOutput

-- | Output from validating an application.
--
--
--
-- /See:/ 'appValidationOutput' smart constructor.
newtype AppValidationOutput = AppValidationOutput'
  { _avoSsmOutput ::
      Maybe SSMOutput
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AppValidationOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avoSsmOutput' - Output from using SSM to validate the application.
appValidationOutput ::
  AppValidationOutput
appValidationOutput = AppValidationOutput' {_avoSsmOutput = Nothing}

-- | Output from using SSM to validate the application.
avoSsmOutput :: Lens' AppValidationOutput (Maybe SSMOutput)
avoSsmOutput = lens _avoSsmOutput (\s a -> s {_avoSsmOutput = a})

instance FromJSON AppValidationOutput where
  parseJSON =
    withObject
      "AppValidationOutput"
      (\x -> AppValidationOutput' <$> (x .:? "ssmOutput"))

instance Hashable AppValidationOutput

instance NFData AppValidationOutput
