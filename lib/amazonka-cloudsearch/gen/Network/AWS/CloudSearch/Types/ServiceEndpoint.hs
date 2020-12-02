{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.ServiceEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.ServiceEndpoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The endpoint to which service requests can be submitted.
--
--
--
-- /See:/ 'serviceEndpoint' smart constructor.
newtype ServiceEndpoint = ServiceEndpoint'
  { _seEndpoint ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ServiceEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'seEndpoint' - Undocumented member.
serviceEndpoint ::
  ServiceEndpoint
serviceEndpoint = ServiceEndpoint' {_seEndpoint = Nothing}

-- | Undocumented member.
seEndpoint :: Lens' ServiceEndpoint (Maybe Text)
seEndpoint = lens _seEndpoint (\s a -> s {_seEndpoint = a})

instance FromXML ServiceEndpoint where
  parseXML x = ServiceEndpoint' <$> (x .@? "Endpoint")

instance Hashable ServiceEndpoint

instance NFData ServiceEndpoint
