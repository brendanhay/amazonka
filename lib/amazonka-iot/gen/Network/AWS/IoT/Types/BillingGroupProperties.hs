{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.BillingGroupProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BillingGroupProperties where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The properties of a billing group.
--
--
--
-- /See:/ 'billingGroupProperties' smart constructor.
newtype BillingGroupProperties = BillingGroupProperties'
  { _bgpBillingGroupDescription ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BillingGroupProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgpBillingGroupDescription' - The description of the billing group.
billingGroupProperties ::
  BillingGroupProperties
billingGroupProperties =
  BillingGroupProperties' {_bgpBillingGroupDescription = Nothing}

-- | The description of the billing group.
bgpBillingGroupDescription :: Lens' BillingGroupProperties (Maybe Text)
bgpBillingGroupDescription = lens _bgpBillingGroupDescription (\s a -> s {_bgpBillingGroupDescription = a})

instance FromJSON BillingGroupProperties where
  parseJSON =
    withObject
      "BillingGroupProperties"
      ( \x ->
          BillingGroupProperties' <$> (x .:? "billingGroupDescription")
      )

instance Hashable BillingGroupProperties

instance NFData BillingGroupProperties

instance ToJSON BillingGroupProperties where
  toJSON BillingGroupProperties' {..} =
    object
      ( catMaybes
          [("billingGroupDescription" .=) <$> _bgpBillingGroupDescription]
      )
