{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.CoreDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.CoreDefinitionVersion where

import Network.AWS.Greengrass.Types.Core
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a core definition version.
--
-- /See:/ 'coreDefinitionVersion' smart constructor.
newtype CoreDefinitionVersion = CoreDefinitionVersion'
  { _cdvCores ::
      Maybe [Core]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CoreDefinitionVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdvCores' - A list of cores in the core definition version.
coreDefinitionVersion ::
  CoreDefinitionVersion
coreDefinitionVersion = CoreDefinitionVersion' {_cdvCores = Nothing}

-- | A list of cores in the core definition version.
cdvCores :: Lens' CoreDefinitionVersion [Core]
cdvCores = lens _cdvCores (\s a -> s {_cdvCores = a}) . _Default . _Coerce

instance FromJSON CoreDefinitionVersion where
  parseJSON =
    withObject
      "CoreDefinitionVersion"
      (\x -> CoreDefinitionVersion' <$> (x .:? "Cores" .!= mempty))

instance Hashable CoreDefinitionVersion

instance NFData CoreDefinitionVersion

instance ToJSON CoreDefinitionVersion where
  toJSON CoreDefinitionVersion' {..} =
    object (catMaybes [("Cores" .=) <$> _cdvCores])
