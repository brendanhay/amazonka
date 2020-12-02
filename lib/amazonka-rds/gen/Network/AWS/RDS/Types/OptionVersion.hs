{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionVersion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The version for an option. Option group option versions are returned by the @DescribeOptionGroupOptions@ action.
--
--
--
-- /See:/ 'optionVersion' smart constructor.
data OptionVersion = OptionVersion'
  { _ovVersion :: !(Maybe Text),
    _ovIsDefault :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OptionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ovVersion' - The version of the option.
--
-- * 'ovIsDefault' - True if the version is the default version of the option, and otherwise false.
optionVersion ::
  OptionVersion
optionVersion =
  OptionVersion' {_ovVersion = Nothing, _ovIsDefault = Nothing}

-- | The version of the option.
ovVersion :: Lens' OptionVersion (Maybe Text)
ovVersion = lens _ovVersion (\s a -> s {_ovVersion = a})

-- | True if the version is the default version of the option, and otherwise false.
ovIsDefault :: Lens' OptionVersion (Maybe Bool)
ovIsDefault = lens _ovIsDefault (\s a -> s {_ovIsDefault = a})

instance FromXML OptionVersion where
  parseXML x =
    OptionVersion' <$> (x .@? "Version") <*> (x .@? "IsDefault")

instance Hashable OptionVersion

instance NFData OptionVersion
