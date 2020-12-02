{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CoipAddressUsage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CoipAddressUsage where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes address usage for a customer-owned address pool.
--
--
--
-- /See:/ 'coipAddressUsage' smart constructor.
data CoipAddressUsage = CoipAddressUsage'
  { _cauAllocationId ::
      !(Maybe Text),
    _cauAWSAccountId :: !(Maybe Text),
    _cauCoIP :: !(Maybe Text),
    _cauAWSService :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CoipAddressUsage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cauAllocationId' - The allocation ID of the address.
--
-- * 'cauAWSAccountId' - The AWS account ID.
--
-- * 'cauCoIP' - The customer-owned IP address.
--
-- * 'cauAWSService' - The AWS service.
coipAddressUsage ::
  CoipAddressUsage
coipAddressUsage =
  CoipAddressUsage'
    { _cauAllocationId = Nothing,
      _cauAWSAccountId = Nothing,
      _cauCoIP = Nothing,
      _cauAWSService = Nothing
    }

-- | The allocation ID of the address.
cauAllocationId :: Lens' CoipAddressUsage (Maybe Text)
cauAllocationId = lens _cauAllocationId (\s a -> s {_cauAllocationId = a})

-- | The AWS account ID.
cauAWSAccountId :: Lens' CoipAddressUsage (Maybe Text)
cauAWSAccountId = lens _cauAWSAccountId (\s a -> s {_cauAWSAccountId = a})

-- | The customer-owned IP address.
cauCoIP :: Lens' CoipAddressUsage (Maybe Text)
cauCoIP = lens _cauCoIP (\s a -> s {_cauCoIP = a})

-- | The AWS service.
cauAWSService :: Lens' CoipAddressUsage (Maybe Text)
cauAWSService = lens _cauAWSService (\s a -> s {_cauAWSService = a})

instance FromXML CoipAddressUsage where
  parseXML x =
    CoipAddressUsage'
      <$> (x .@? "allocationId")
      <*> (x .@? "awsAccountId")
      <*> (x .@? "coIp")
      <*> (x .@? "awsService")

instance Hashable CoipAddressUsage

instance NFData CoipAddressUsage
