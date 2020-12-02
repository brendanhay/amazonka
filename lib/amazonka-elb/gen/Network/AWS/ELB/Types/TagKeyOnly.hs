{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.Types.TagKeyOnly
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.TagKeyOnly where

import Network.AWS.ELB.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The key of a tag.
--
--
--
-- /See:/ 'tagKeyOnly' smart constructor.
newtype TagKeyOnly = TagKeyOnly' {_tkoKey :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TagKeyOnly' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tkoKey' - The name of the key.
tagKeyOnly ::
  TagKeyOnly
tagKeyOnly = TagKeyOnly' {_tkoKey = Nothing}

-- | The name of the key.
tkoKey :: Lens' TagKeyOnly (Maybe Text)
tkoKey = lens _tkoKey (\s a -> s {_tkoKey = a})

instance Hashable TagKeyOnly

instance NFData TagKeyOnly

instance ToQuery TagKeyOnly where
  toQuery TagKeyOnly' {..} = mconcat ["Key" =: _tkoKey]
