{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourcePath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ResourcePath where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.ResourcePathComponent

-- | Describes the path information of a resource.
--
--
--
-- /See:/ 'resourcePath' smart constructor.
newtype ResourcePath = ResourcePath'
  { _rpComponents ::
      Maybe [ResourcePathComponent]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourcePath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpComponents' - The components of the resource path.
resourcePath ::
  ResourcePath
resourcePath = ResourcePath' {_rpComponents = Nothing}

-- | The components of the resource path.
rpComponents :: Lens' ResourcePath [ResourcePathComponent]
rpComponents = lens _rpComponents (\s a -> s {_rpComponents = a}) . _Default . _Coerce

instance FromJSON ResourcePath where
  parseJSON =
    withObject
      "ResourcePath"
      (\x -> ResourcePath' <$> (x .:? "Components" .!= mempty))

instance Hashable ResourcePath

instance NFData ResourcePath
