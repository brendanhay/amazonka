{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.InstanceIdDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InstanceIdDetail where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of EC2 instance IDs corresponding to the segments in a trace.
--
--
--
-- /See:/ 'instanceIdDetail' smart constructor.
newtype InstanceIdDetail = InstanceIdDetail' {_iidId :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceIdDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iidId' - The ID of a corresponding EC2 instance.
instanceIdDetail ::
  InstanceIdDetail
instanceIdDetail = InstanceIdDetail' {_iidId = Nothing}

-- | The ID of a corresponding EC2 instance.
iidId :: Lens' InstanceIdDetail (Maybe Text)
iidId = lens _iidId (\s a -> s {_iidId = a})

instance FromJSON InstanceIdDetail where
  parseJSON =
    withObject
      "InstanceIdDetail"
      (\x -> InstanceIdDetail' <$> (x .:? "Id"))

instance Hashable InstanceIdDetail

instance NFData InstanceIdDetail
