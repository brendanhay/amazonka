{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ResourceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ResourceDetails where

import Network.AWS.CostExplorer.Types.EC2ResourceDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details on the resource.
--
--
--
-- /See:/ 'resourceDetails' smart constructor.
newtype ResourceDetails = ResourceDetails'
  { _rdEC2ResourceDetails ::
      Maybe EC2ResourceDetails
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdEC2ResourceDetails' - Details on the Amazon EC2 resource.
resourceDetails ::
  ResourceDetails
resourceDetails = ResourceDetails' {_rdEC2ResourceDetails = Nothing}

-- | Details on the Amazon EC2 resource.
rdEC2ResourceDetails :: Lens' ResourceDetails (Maybe EC2ResourceDetails)
rdEC2ResourceDetails = lens _rdEC2ResourceDetails (\s a -> s {_rdEC2ResourceDetails = a})

instance FromJSON ResourceDetails where
  parseJSON =
    withObject
      "ResourceDetails"
      (\x -> ResourceDetails' <$> (x .:? "EC2ResourceDetails"))

instance Hashable ResourceDetails

instance NFData ResourceDetails
