{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ScriptBootstrapActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScriptBootstrapActionConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration of the script to run during a bootstrap action.
--
--
--
-- /See:/ 'scriptBootstrapActionConfig' smart constructor.
data ScriptBootstrapActionConfig = ScriptBootstrapActionConfig'
  { _sbacArgs ::
      !(Maybe [Text]),
    _sbacPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScriptBootstrapActionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sbacArgs' - A list of command line arguments to pass to the bootstrap action script.
--
-- * 'sbacPath' - Location of the script to run during a bootstrap action. Can be either a location in Amazon S3 or on a local file system.
scriptBootstrapActionConfig ::
  -- | 'sbacPath'
  Text ->
  ScriptBootstrapActionConfig
scriptBootstrapActionConfig pPath_ =
  ScriptBootstrapActionConfig'
    { _sbacArgs = Nothing,
      _sbacPath = pPath_
    }

-- | A list of command line arguments to pass to the bootstrap action script.
sbacArgs :: Lens' ScriptBootstrapActionConfig [Text]
sbacArgs = lens _sbacArgs (\s a -> s {_sbacArgs = a}) . _Default . _Coerce

-- | Location of the script to run during a bootstrap action. Can be either a location in Amazon S3 or on a local file system.
sbacPath :: Lens' ScriptBootstrapActionConfig Text
sbacPath = lens _sbacPath (\s a -> s {_sbacPath = a})

instance Hashable ScriptBootstrapActionConfig

instance NFData ScriptBootstrapActionConfig

instance ToJSON ScriptBootstrapActionConfig where
  toJSON ScriptBootstrapActionConfig' {..} =
    object
      (catMaybes [("Args" .=) <$> _sbacArgs, Just ("Path" .= _sbacPath)])
