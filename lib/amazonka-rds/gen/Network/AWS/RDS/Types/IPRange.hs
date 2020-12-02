{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.IPRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.IPRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type is used as a response element in the @DescribeDBSecurityGroups@ action.
--
--
--
-- /See:/ 'ipRange' smart constructor.
data IPRange = IPRange'
  { _irStatus :: !(Maybe Text),
    _irCIdRIP :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'IPRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'irStatus' - Specifies the status of the IP range. Status can be "authorizing", "authorized", "revoking", and "revoked".
--
-- * 'irCIdRIP' - Specifies the IP range.
ipRange ::
  IPRange
ipRange = IPRange' {_irStatus = Nothing, _irCIdRIP = Nothing}

-- | Specifies the status of the IP range. Status can be "authorizing", "authorized", "revoking", and "revoked".
irStatus :: Lens' IPRange (Maybe Text)
irStatus = lens _irStatus (\s a -> s {_irStatus = a})

-- | Specifies the IP range.
irCIdRIP :: Lens' IPRange (Maybe Text)
irCIdRIP = lens _irCIdRIP (\s a -> s {_irCIdRIP = a})

instance FromXML IPRange where
  parseXML x = IPRange' <$> (x .@? "Status") <*> (x .@? "CIDRIP")

instance Hashable IPRange

instance NFData IPRange
