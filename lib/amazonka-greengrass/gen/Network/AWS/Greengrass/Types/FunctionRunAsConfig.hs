{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionRunAsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionRunAsConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the user and group whose permissions are used when running the Lambda function. You can specify one or both values to override the default values. We recommend that you avoid running as root unless absolutely necessary to minimize the risk of unintended changes or malicious attacks. To run as root, you must set ''IsolationMode'' to ''NoContainer'' and update config.json in ''greengrass-root/config'' to set ''allowFunctionsToRunAsRoot'' to ''yes''.
--
-- /See:/ 'functionRunAsConfig' smart constructor.
data FunctionRunAsConfig = FunctionRunAsConfig'
  { _fracUid ::
      !(Maybe Int),
    _fracGid :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FunctionRunAsConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fracUid' - The user ID whose permissions are used to run a Lambda function.
--
-- * 'fracGid' - The group ID whose permissions are used to run a Lambda function.
functionRunAsConfig ::
  FunctionRunAsConfig
functionRunAsConfig =
  FunctionRunAsConfig' {_fracUid = Nothing, _fracGid = Nothing}

-- | The user ID whose permissions are used to run a Lambda function.
fracUid :: Lens' FunctionRunAsConfig (Maybe Int)
fracUid = lens _fracUid (\s a -> s {_fracUid = a})

-- | The group ID whose permissions are used to run a Lambda function.
fracGid :: Lens' FunctionRunAsConfig (Maybe Int)
fracGid = lens _fracGid (\s a -> s {_fracGid = a})

instance FromJSON FunctionRunAsConfig where
  parseJSON =
    withObject
      "FunctionRunAsConfig"
      (\x -> FunctionRunAsConfig' <$> (x .:? "Uid") <*> (x .:? "Gid"))

instance Hashable FunctionRunAsConfig

instance NFData FunctionRunAsConfig

instance ToJSON FunctionRunAsConfig where
  toJSON FunctionRunAsConfig' {..} =
    object
      (catMaybes [("Uid" .=) <$> _fracUid, ("Gid" .=) <$> _fracGid])
