{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.Resource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.Resource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a resource for a project.
--
--
--
-- /See:/ 'resource' smart constructor.
newtype Resource = Resource' {_rId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Resource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rId' - The Amazon Resource Name (ARN) of the resource.
resource ::
  -- | 'rId'
  Text ->
  Resource
resource pId_ = Resource' {_rId = pId_}

-- | The Amazon Resource Name (ARN) of the resource.
rId :: Lens' Resource Text
rId = lens _rId (\s a -> s {_rId = a})

instance FromJSON Resource where
  parseJSON =
    withObject "Resource" (\x -> Resource' <$> (x .: "id"))

instance Hashable Resource

instance NFData Resource
