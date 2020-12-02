{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourcePathComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ResourcePathComponent where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the resource path.
--
--
--
-- /See:/ 'resourcePathComponent' smart constructor.
data ResourcePathComponent = ResourcePathComponent'
  { _rpcName ::
      !(Maybe Text),
    _rpcId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourcePathComponent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpcName' - The name of the resource path.
--
-- * 'rpcId' - The ID of the resource path.
resourcePathComponent ::
  ResourcePathComponent
resourcePathComponent =
  ResourcePathComponent' {_rpcName = Nothing, _rpcId = Nothing}

-- | The name of the resource path.
rpcName :: Lens' ResourcePathComponent (Maybe Text)
rpcName = lens _rpcName (\s a -> s {_rpcName = a})

-- | The ID of the resource path.
rpcId :: Lens' ResourcePathComponent (Maybe Text)
rpcId = lens _rpcId (\s a -> s {_rpcId = a})

instance FromJSON ResourcePathComponent where
  parseJSON =
    withObject
      "ResourcePathComponent"
      (\x -> ResourcePathComponent' <$> (x .:? "Name") <*> (x .:? "Id"))

instance Hashable ResourcePathComponent

instance NFData ResourcePathComponent
