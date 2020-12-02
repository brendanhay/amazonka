{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ConfigurationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ConfigurationSet where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The name of the configuration set.
--
--
-- Configuration sets let you create groups of rules that you can apply to the emails you send using Amazon SES. For more information about using configuration sets, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-configuration-sets.html Using Amazon SES Configuration Sets> in the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/ Amazon SES Developer Guide> .
--
--
-- /See:/ 'configurationSet' smart constructor.
newtype ConfigurationSet = ConfigurationSet' {_csName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ConfigurationSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csName' - The name of the configuration set. The name must meet the following requirements:     * Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain 64 characters or fewer.
configurationSet ::
  -- | 'csName'
  Text ->
  ConfigurationSet
configurationSet pName_ = ConfigurationSet' {_csName = pName_}

-- | The name of the configuration set. The name must meet the following requirements:     * Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).     * Contain 64 characters or fewer.
csName :: Lens' ConfigurationSet Text
csName = lens _csName (\s a -> s {_csName = a})

instance FromXML ConfigurationSet where
  parseXML x = ConfigurationSet' <$> (x .@ "Name")

instance Hashable ConfigurationSet

instance NFData ConfigurationSet

instance ToQuery ConfigurationSet where
  toQuery ConfigurationSet' {..} = mconcat ["Name" =: _csName]
