{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEnclaveOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateEnclaveOptions where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether the instance is enabled for AWS Nitro Enclaves.
--
--
--
-- /See:/ 'launchTemplateEnclaveOptions' smart constructor.
newtype LaunchTemplateEnclaveOptions = LaunchTemplateEnclaveOptions'
  { _lteoEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateEnclaveOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lteoEnabled' - If this parameter is set to @true@ , the instance is enabled for AWS Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
launchTemplateEnclaveOptions ::
  LaunchTemplateEnclaveOptions
launchTemplateEnclaveOptions =
  LaunchTemplateEnclaveOptions' {_lteoEnabled = Nothing}

-- | If this parameter is set to @true@ , the instance is enabled for AWS Nitro Enclaves; otherwise, it is not enabled for AWS Nitro Enclaves.
lteoEnabled :: Lens' LaunchTemplateEnclaveOptions (Maybe Bool)
lteoEnabled = lens _lteoEnabled (\s a -> s {_lteoEnabled = a})

instance FromXML LaunchTemplateEnclaveOptions where
  parseXML x = LaunchTemplateEnclaveOptions' <$> (x .@? "enabled")

instance Hashable LaunchTemplateEnclaveOptions

instance NFData LaunchTemplateEnclaveOptions
