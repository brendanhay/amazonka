{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes whether S3 data event logs will be automatically enabled for new members of the organization.
--
--
--
-- /See:/ 'organizationS3LogsConfiguration' smart constructor.
newtype OrganizationS3LogsConfiguration = OrganizationS3LogsConfiguration'
  { _oslcAutoEnable ::
      Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OrganizationS3LogsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'oslcAutoEnable' - A value that contains information on whether S3 data event logs will be enabled automatically as a data source for the organization.
organizationS3LogsConfiguration ::
  -- | 'oslcAutoEnable'
  Bool ->
  OrganizationS3LogsConfiguration
organizationS3LogsConfiguration pAutoEnable_ =
  OrganizationS3LogsConfiguration' {_oslcAutoEnable = pAutoEnable_}

-- | A value that contains information on whether S3 data event logs will be enabled automatically as a data source for the organization.
oslcAutoEnable :: Lens' OrganizationS3LogsConfiguration Bool
oslcAutoEnable = lens _oslcAutoEnable (\s a -> s {_oslcAutoEnable = a})

instance Hashable OrganizationS3LogsConfiguration

instance NFData OrganizationS3LogsConfiguration

instance ToJSON OrganizationS3LogsConfiguration where
  toJSON OrganizationS3LogsConfiguration' {..} =
    object (catMaybes [Just ("autoEnable" .= _oslcAutoEnable)])
