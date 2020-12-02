{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.BillingGroupMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.BillingGroupMetadata where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Additional information about the billing group.
--
--
--
-- /See:/ 'billingGroupMetadata' smart constructor.
newtype BillingGroupMetadata = BillingGroupMetadata'
  { _bgmCreationDate ::
      Maybe POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BillingGroupMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgmCreationDate' - The date the billing group was created.
billingGroupMetadata ::
  BillingGroupMetadata
billingGroupMetadata =
  BillingGroupMetadata' {_bgmCreationDate = Nothing}

-- | The date the billing group was created.
bgmCreationDate :: Lens' BillingGroupMetadata (Maybe UTCTime)
bgmCreationDate = lens _bgmCreationDate (\s a -> s {_bgmCreationDate = a}) . mapping _Time

instance FromJSON BillingGroupMetadata where
  parseJSON =
    withObject
      "BillingGroupMetadata"
      (\x -> BillingGroupMetadata' <$> (x .:? "creationDate"))

instance Hashable BillingGroupMetadata

instance NFData BillingGroupMetadata
