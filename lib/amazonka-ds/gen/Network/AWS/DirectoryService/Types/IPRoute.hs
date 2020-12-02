{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.IPRoute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.IPRoute where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | IP address block. This is often the address block of the DNS server used for your on-premises domain.
--
--
--
-- /See:/ 'ipRoute' smart constructor.
data IPRoute = IPRoute'
  { _irCidrIP :: !(Maybe Text),
    _irDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPRoute' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irCidrIP' - IP address block using CIDR format, for example 10.0.0.0/24. This is often the address block of the DNS server used for your on-premises domain. For a single IP address use a CIDR address block with /32. For example 10.0.0.0/32.
--
-- * 'irDescription' - Description of the address block.
ipRoute ::
  IPRoute
ipRoute = IPRoute' {_irCidrIP = Nothing, _irDescription = Nothing}

-- | IP address block using CIDR format, for example 10.0.0.0/24. This is often the address block of the DNS server used for your on-premises domain. For a single IP address use a CIDR address block with /32. For example 10.0.0.0/32.
irCidrIP :: Lens' IPRoute (Maybe Text)
irCidrIP = lens _irCidrIP (\s a -> s {_irCidrIP = a})

-- | Description of the address block.
irDescription :: Lens' IPRoute (Maybe Text)
irDescription = lens _irDescription (\s a -> s {_irDescription = a})

instance Hashable IPRoute

instance NFData IPRoute

instance ToJSON IPRoute where
  toJSON IPRoute' {..} =
    object
      ( catMaybes
          [ ("CidrIp" .=) <$> _irCidrIP,
            ("Description" .=) <$> _irDescription
          ]
      )
