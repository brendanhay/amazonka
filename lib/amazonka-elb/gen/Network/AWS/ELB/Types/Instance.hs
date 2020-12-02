{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.Instance where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The ID of an EC2 instance.
--
--
--
-- /See:/ 'instance'' smart constructor.
newtype Instance = Instance' {_iInstanceId :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iInstanceId' - The instance ID.
instance' ::
  Instance
instance' = Instance' {_iInstanceId = Nothing}

-- | The instance ID.
iInstanceId :: Lens' Instance (Maybe Text)
iInstanceId = lens _iInstanceId (\s a -> s {_iInstanceId = a})

instance FromXML Instance where
  parseXML x = Instance' <$> (x .@? "InstanceId")

instance Hashable Instance

instance NFData Instance

instance ToQuery Instance where
  toQuery Instance' {..} = mconcat ["InstanceId" =: _iInstanceId]
