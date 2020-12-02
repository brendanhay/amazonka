{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Instance where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The description of an Amazon EC2 instance.
--
--
--
-- /See:/ 'instance'' smart constructor.
newtype Instance = Instance' {_iId :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iId' - The ID of the Amazon EC2 instance.
instance' ::
  Instance
instance' = Instance' {_iId = Nothing}

-- | The ID of the Amazon EC2 instance.
iId :: Lens' Instance (Maybe Text)
iId = lens _iId (\s a -> s {_iId = a})

instance FromXML Instance where
  parseXML x = Instance' <$> (x .@? "Id")

instance Hashable Instance

instance NFData Instance
