{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.StackConfigurationManager
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.StackConfigurationManager where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the configuration manager.
--
--
--
-- /See:/ 'stackConfigurationManager' smart constructor.
data StackConfigurationManager = StackConfigurationManager'
  { _scmName ::
      !(Maybe Text),
    _scmVersion :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StackConfigurationManager' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scmName' - The name. This parameter must be set to "Chef".
--
-- * 'scmVersion' - The Chef version. This parameter must be set to 12, 11.10, or 11.4 for Linux stacks, and to 12.2 for Windows stacks. The default value for Linux stacks is 11.4.
stackConfigurationManager ::
  StackConfigurationManager
stackConfigurationManager =
  StackConfigurationManager'
    { _scmName = Nothing,
      _scmVersion = Nothing
    }

-- | The name. This parameter must be set to "Chef".
scmName :: Lens' StackConfigurationManager (Maybe Text)
scmName = lens _scmName (\s a -> s {_scmName = a})

-- | The Chef version. This parameter must be set to 12, 11.10, or 11.4 for Linux stacks, and to 12.2 for Windows stacks. The default value for Linux stacks is 11.4.
scmVersion :: Lens' StackConfigurationManager (Maybe Text)
scmVersion = lens _scmVersion (\s a -> s {_scmVersion = a})

instance FromJSON StackConfigurationManager where
  parseJSON =
    withObject
      "StackConfigurationManager"
      ( \x ->
          StackConfigurationManager'
            <$> (x .:? "Name") <*> (x .:? "Version")
      )

instance Hashable StackConfigurationManager

instance NFData StackConfigurationManager

instance ToJSON StackConfigurationManager where
  toJSON StackConfigurationManager' {..} =
    object
      ( catMaybes
          [("Name" .=) <$> _scmName, ("Version" .=) <$> _scmVersion]
      )
