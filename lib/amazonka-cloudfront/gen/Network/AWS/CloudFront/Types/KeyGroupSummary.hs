{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.KeyGroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.KeyGroupSummary where

import Network.AWS.CloudFront.Types.KeyGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a key group.
--
--
--
-- /See:/ 'keyGroupSummary' smart constructor.
newtype KeyGroupSummary = KeyGroupSummary'
  { _kgsKeyGroup ::
      KeyGroup
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyGroupSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kgsKeyGroup' - A key group.
keyGroupSummary ::
  -- | 'kgsKeyGroup'
  KeyGroup ->
  KeyGroupSummary
keyGroupSummary pKeyGroup_ =
  KeyGroupSummary' {_kgsKeyGroup = pKeyGroup_}

-- | A key group.
kgsKeyGroup :: Lens' KeyGroupSummary KeyGroup
kgsKeyGroup = lens _kgsKeyGroup (\s a -> s {_kgsKeyGroup = a})

instance FromXML KeyGroupSummary where
  parseXML x = KeyGroupSummary' <$> (x .@ "KeyGroup")

instance Hashable KeyGroupSummary

instance NFData KeyGroupSummary
