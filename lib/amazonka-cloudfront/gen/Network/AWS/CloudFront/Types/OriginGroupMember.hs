{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.OriginGroupMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.OriginGroupMember where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An origin in an origin group.
--
--
--
-- /See:/ 'originGroupMember' smart constructor.
newtype OriginGroupMember = OriginGroupMember'
  { _ogmOriginId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OriginGroupMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogmOriginId' - The ID for an origin in an origin group.
originGroupMember ::
  -- | 'ogmOriginId'
  Text ->
  OriginGroupMember
originGroupMember pOriginId_ =
  OriginGroupMember' {_ogmOriginId = pOriginId_}

-- | The ID for an origin in an origin group.
ogmOriginId :: Lens' OriginGroupMember Text
ogmOriginId = lens _ogmOriginId (\s a -> s {_ogmOriginId = a})

instance FromXML OriginGroupMember where
  parseXML x = OriginGroupMember' <$> (x .@ "OriginId")

instance Hashable OriginGroupMember

instance NFData OriginGroupMember

instance ToXML OriginGroupMember where
  toXML OriginGroupMember' {..} = mconcat ["OriginId" @= _ogmOriginId]
